/*  Part of Static Checks

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/stchecks
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(check_non_mutually_exclusive, []).

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(pairs)).
:- use_module(library(checker)).
:- use_module(library(check), []).
:- use_module(library(clambda)).
:- use_module(library(normalize_head)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(referenced_by)).
:- use_module(library(from_utils)).
:- init_expansors.

:- multifile
    prolog:message//1,
    mutually_exclusive_predicate/2,
    mutually_exclusive_predicate_key/3,
    mutually_exclusive_predicate_key/4.

mutually_exclusive_predicate(MH) :-
    ( var(MH)
    ->MH = M:H
    ; strip_module(MH, M, H)
    ),
    mutually_exclusive_predicate(H, M).

mutually_exclusive_predicate(check(_, _, _), checker).
mutually_exclusive_predicate_key(check(K, _, _), checker, K).

checker:check(non_mutually_exclusive, Result, Options) :-
    option_module_files(Options, MFileD),
    option(predicate(Ref), Options, Ref),
    findall(Pairs, check_non_mutually_exclusive(MFileD, Ref, Pairs), Result).

check_non_mutually_exclusive(MFileD, Ref, warning-(Ref-LocIdx)) :-
    normalize_head(Ref, MH),
    mutually_exclusive_predicate(MH),
    collect_non_mutually_exclusive(MFileD, MH, LocIdxL),
    member(LocIdx, LocIdxL).

cleanup_redundant_groups([], _, []).
cleanup_redundant_groups([Key-Clause-ClauseNME|ClauseKeyU], ClauseKeyI, ClauseKeyR) :-
    ( \+ ( member(Key2-Clause2-ClauseNME2, ClauseKeyU),
           subset([Key-Clause|ClauseNME], [Key2-Clause2|ClauseNME2]),
           \+subset([Key2-Clause2|ClauseNME2], [Key-Clause|ClauseNME])
         ),
      \+ ( member(Key2-Clause2-ClauseNME2, ClauseKeyI),
           subset([Key-Clause|ClauseNME], [Key2-Clause2|ClauseNME2])
         )
    ->ClauseKeyR=[Key-Clause-ClauseNME|ClauseKeyR2],
      ClauseKeyI2=[Key-Clause-ClauseNME|ClauseKeyI]
    ; ClauseKeyR=ClauseKeyR2,
      ClauseKeyI2=ClauseKeyI
    ),
    cleanup_redundant_groups(ClauseKeyU, ClauseKeyI2, ClauseKeyR2).

:- meta_predicate collect_non_mutually_exclusive(+, 0, -).

collect_non_mutually_exclusive(MFileD, MH, LocPL) :-
    strip_module(MH, M, H),
    get_dict(M, MFileD, FileD),
    findall(I-(Key-Clause),
            ( clause(MH, B, Clause),
              nth_clause(MH, I, Clause),
              From = clause(Clause),
              from_to_file(From, File),
              get_dict(File, FileD, _),
              ( mutually_exclusive_predicate_key(H, M, Key)
              ->true
              ; mutually_exclusive_predicate_key(H, M, B, Key)
              ->true
              ; Key = MH
              )
            ),
            ClauseKeyU),
    ClauseKeyU \= [],
    list_to_ord_set(ClauseKeyU, ClauseKeyL),
    findall(Key-Clause-ClauseNME,
            [Clause, Key, ClauseNME, ClauseKeyL] +\
            ( select(_-(Key-Clause), ClauseKeyL, ClauseKeyS),
              exclude([Key] +\ (_-(SKey-_)) ^ (SKey\=Key), ClauseKeyS, ClauseKeyNME),
              ClauseKeyNME \= [],
              pairs_values(ClauseKeyNME, ClauseNME)
            ),
            ClausePR),
    cleanup_redundant_groups(ClausePR, [], ClausePL),
    maplist(\ (Key1-Clause1-ClauseNME1)^((Loc1-Idx1/Key1)/LocL)
           ^( nth_clause(_, Idx1, Clause1),
              from_location(clause(Clause1), Loc1),
              maplist(\ (Key2-Clause2)^(Loc2-Idx2/Key2)
                     ^( nth_clause(_, Idx2, Clause2),
                        from_location(clause(Clause2), Loc2)
                      ),
                      ClauseNME1,
                      LocU),
              sort(LocU, LocL)
            ), ClausePL, LocPU),
    sort(LocPU, LocPL).

prolog:message(acheck(non_mutually_exclusive)) -->
    ['Non Mutually Exclusive Predicates',nl,
     '---------------------------------',nl,
     'The predicates below are marked as mutually_exclusive, but they have', nl,
     'non mutually exclusive clauses. You can resolve the ambiguity unifying', nl,
     'the non mutual exclusive clauses or changing the specification of such', nl,
     'predicates.', nl, nl].
prolog:message(acheck(non_mutually_exclusive, PI-LocCIs)) -->
    check:predicate(PI),
    [' has non mutually exclusive clauses:', nl],
    foldl(group_non_mut_ex, LocCIs).

locindex_index(_-I/_, I).

locindex_loccl(Loc-I/K, Loc/[' clause ~w/~w'-[I, K]]).

group_non_mut_ex((Loc-Idx/Key)/LocIdxL) -->
    {maplist(locindex_index, LocIdxL, Idxs)},
    {maplist(locindex_loccl, LocIdxL, LCIs)},
    Loc, ['Clause ~w (key ~w) match with clauses ~w at'-[Idx, Key, Idxs], nl],
    referenced_by(LCIs).
