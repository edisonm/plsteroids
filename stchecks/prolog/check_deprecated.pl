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

:- module(check_deprecated, []).

:- use_module(library(apply)).
:- use_module(library(option)).
:- use_module(library(checker)).
:- use_module(library(globprops)).
:- use_module(library(check), []).
:- use_module(library(location_utils)).
:- use_module(library(referenced_by)).
:- use_module(library(assertions)).
:- use_module(library(codewalk)).
:- use_module(library(clause_indicator)).
:- use_module(library(from_utils)).
:- init_expansors.

:- dynamic
    deprecated_db/7.

:- multifile
    prolog:message//1,
    deprecated_predicate/3.

deprecated_predicate(MGoal, Comment, DFrom, CFrom) :-
    prop_asr(head, MGoal, DFrom, Asr),
    prop_asr(glob, deprecated(_), _, Asr),
    ( curr_prop_asr(comm, Comment, CFrom, Asr)
    ->true
    ; Comment = "",
      CFrom = DFrom
    ).
deprecated_predicate(M:Goal, AltMsg, [], []) :-
    predicate_property(M:Goal, implementation_module(IM)),
    deprecated_predicate(Goal, IM, AltMsg).

checker:check(deprecated, Result, Options) :-
    check_deprecated(Options, Result).

check_deprecated(Options1, Pairs) :-
    merge_options(Options1,
                  [source(true),
                   infer_meta_predicates(false),
                   autoload(false),
                   evaluate(false),
                   trace_reference(_),
                   on_trace(collect_deprecated)],
                  Options),
    walk_code(Options),
    findall(information-((DLoc/(IM:F/A))-((CLoc/Comment)-(Loc/CI))),
            ( retract(deprecated_db(Call, M, Caller, Comment, DFrom, CFrom, From)),
              predicate_property(M:Call, implementation_module(IM)),
              functor(Call, F, A),
              from_location(DFrom, DLoc),
              from_location(CFrom, CLoc),
              from_location(From, Loc),
              clause_indicator(From, Caller, CI, [])
            ), Pairs).

prolog:message(acheck(deprecated)) -->
    ['Deprecated Predicates',nl,
     '---------------------',nl,
     'The predicates below are marked as deprecated, so you have to', nl,
     'avoid its usage in new code, and to refactorize old code.', nl, nl].
prolog:message(acheck(deprecated, (Loc/PI)-CommentLocCIL)) -->
    Loc,
    ["~w deprecated."-[PI], nl],
    foldl(comment_referenced_by, CommentLocCIL).

comment_referenced_by((Loc/Comment)-LocCIL) -->
    ["    "], Loc, [Comment, ". Referenced by", nl],
    referenced_by(LocCIL).

:- public collect_deprecated/3.
:- meta_predicate collect_deprecated(0,?,?).
collect_deprecated(M:Goal, Caller, From) :-
    deprecated_predicate(M:Goal, Comment, DFrom, CFrom),
    % counter intuitive optimization: we save M (context module) instead of
    % implementation module since M is more discriminative
    update_fact_from(deprecated_db(Goal, M, Caller, Comment, DFrom, CFrom), From).
