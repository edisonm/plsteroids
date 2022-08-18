/*  Part of Static Checks

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/stchecks
    Copyright (C): 2020, Process Design Center, Breda, The Netherlands.
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

:- module(check_module_loops, []).

:- use_module(library(lists)).
:- use_module(library(calls_to)).
:- use_module(library(checker)).
:- use_module(library(module_loops)).
:- use_module(library(module_links)).
:- use_module(library(extra_location)).
:- use_module(library(location_utils)).

:- multifile
        prolog:message//1.

prolog:message(acheck(module_loops)) -->
    ['Module loops', nl,
     '------------', nl,
     'Module loops could potentially lead to Demeter\'s law violations.', nl,
     'To help mitigate the problem, this analysis reports the predicates', nl,
     'that can be reorganized in order to break the loop, as suggestion,', nl,
     'focus first on the predicates reported as `used by`.  If is not', nl,
     'possible to decouple the module without changing the predicates,', nl,
     'it is reported as a strong module loop and will require further', nl,
     'refactoring or module merge to decouple the involved code.  If the', nl,
     'list of strongly connected predicates is empty, it is possible to', nl,
     'resolve the loop involving more than 2 modules and it is reported', nl,
     'as a complex module loop.', nl, nl].

prolog:message(acheck(module_loops, Issue)) -->
    module_loops_message_type(Issue).

module_loops_message_type(l(Loc/Loop)-[UnlinkL]) -->
    Loc,
    ['Module loop found ~w, it can be broken at:'-[Loop], nl],
    foldl(ml_msg_can_be_broken_at, UnlinkL).
module_loops_message_type(m(Loc/Loop)-[PredL]) -->
    Loc,
    ['Complex module loop found ~w, involved predicates are ~w'-[Loop, PredL], nl].
module_loops_message_type(s(Loc/Loop)-[StrongL]) -->
    Loc,
    ['Strong module loop found ~w, involved predicates are ~w'-[Loop, StrongL], nl].

ml_msg_can_be_broken_at([M2, Loc1, M1, PL1, L1, Loc3, M3, PL3, L3]) -->
    ml_msg_alternative_paths(M2, 'used by', Loc1, M1, PL1, L1),
    ml_msg_alternative_paths(M2, 'uses',    Loc3, M3, PL3, L3).

ml_msg_alternative_paths(M2, Label, Loc, M, PL, L) -->
    ['\t'|Loc], ["~w ~w ~w: ~w"-[M2, Label, M, L], nl],
    foldl(ml_msg_alternative_path, PL).

ml_msg_alternative_path(P) -->
    ["\t\tAlternative path: ~w"-[P], nl].

checker:check(module_loops, Pairs, Options) :-
    collect_calls_to(Options, _),
    update_depends_of,
    module_loops(Loops, Options),
    findall(Pair, loops_pairs(Loops, Pair), Pairs).

%!  collect_module_pred_paths(+Direction, Module1, Module2, PL, P2L) is det.

%   If Direction if forw, P2L is the list of predicates in Module2 that Module1
%   depends on, if Direction is back, P2L is the list of predicates in Module2
%   that depends on Module1.  PL is the list of alternative chains that connects
%   Module2 with Module1.

collect_module_pred_paths(D, M1, M2, PL, P2L) :-
    findall(PILL-PIL,
            ( ( C = []
              ; C = [I|P],
                loads_db(M1, I, _, 1),
                loads_db(I, M2, P, _),
                \+ memberchk(M1, P)
              ),
              module_pred_chains(D, M1, M2, C, PILL, PIL)
            ), Pairs),
    pairs_keys_values(Pairs, [_|PL], P2LL),
    append(P2LL, P2U),
    sort(P2U, P2L).

loop_breakable_chain(Loop, [M2, Loc1, M1, PL1, L1, Loc3, M3, PL3, L3]) :-
    loop_to_chain(Loop, Chain),
    current_chain_link(Chain, M1, M2, M3),
    collect_module_pred_paths(forw, M1, M2, PL1, PL12), % M2 used by M1
    collect_module_pred_paths(back, M2, M3, PL3, PL23), % M2 uses M3
    % The intersection of PL12 and PL23 gives the list of predicates in M2 used
    % in M1 that depends on M3, and therefore prevents the independence of such
    % modules.  So we can break such dependency if such intersection is empty:
    intersection(PL12, PL23, []),
    preds_uses(M2, PL12, L1),
    preds_uses(M2, PL23, L3),
    guess_link_location([M1, M2], Loc1),
    guess_link_location([M2, M3], Loc3).

loops_pairs(Loops, warning-Issue) :-
    member(Loop, Loops),
    guess_link_location(Loop, Loc),
    findall(Unlink, loop_breakable_chain(Loop, Unlink), UnlinkL),
    ( UnlinkL \= []
    ->Issue = l(Loc/Loop)-UnlinkL
    ; module_pred_links(Loop, PredLinksL),
      ( PredLinksL = [_:[]|_]
      ->Issue = m(Loc/Loop)-PredLinksL
      ; Issue = s(Loc/Loop)-PredLinksL
      )
    ).

guess_link_location(Loop, Loc) :-
    Loop = [LoadedIn|List],
    ( ( loc_declaration(           Alias,     LoadedIn, use_module,   From)
      ; loc_declaration(use_module(Alias, _), LoadedIn, use_module_2, From)
      ),
      absolute_file_name(Alias, AF, [file_errors(fail), file_type(prolog)]),
      ( List = []
      ; List = [Module|_],
        module_property(Module, file(File)),
        File == AF
      )
    ->from_location(From, Loc)
    ; Loc = []
    ).
