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
     'that can be reorganized in order to break the loop. If that is not', nl,
     'possible, it is reported as a strong module loop and will require', nl,
     'further refactoring or module merge to decouple the involved code.', nl,
     'If the list of the strongly connected predicate is empty, it means', nl,
     'that is possible to resolve the loop involving more than 2 modules', nl,
     'and it will be reported as a complex module loop.', nl, nl].

prolog:message(acheck(module_loops, Issue)) -->
    module_loops_message_type(Issue).

module_loops_message_type(l(Loc/Loop)-[UnlinkL]) -->
    Loc,
    ['Module loop found ~w, it can be broken at ~w'-[Loop, UnlinkL], nl].
module_loops_message_type(m(Loc/Loop)-[PredL]) -->
    Loc,
    ['Complex module loop found ~w, involved predicates are ~w'-[Loop, PredL], nl].
module_loops_message_type(s(Loc/Loop)-[StrongL]) -->
    Loc,
    ['Strong module loop found ~w, involved predicates are ~w'-[Loop, StrongL], nl].
module_loops_message_type(u(Loc/M)-IssueL) -->
    Loc,
    ['Module ~w can be splitted to decouple indirectly linked modules'-[M], nl],
    foldl(module_loops_unlink_message(M), IssueL).

type_label(l, 'used by').
type_label(r, 'uses').

module_loops_unlink_message(M, LocL/(M1:L1/Type)) -->
    {type_label(Type, Label)},
    ['\t'|LocL], ['From ~w ~w ~w: ~w'-[M, Label, M1, L1], nl].

checker:check(module_loops, Pairs, Options) :-
    collect_calls_to(Options, _),
    update_depends_of,
    module_loops(Loops, Options),
    findall(Pair, loops_pairs(Loops, Pair), Pairs).

loops_pairs(Loops, warning-Issue) :-
    member(Loop, Loops),
    guess_loop_location(Loop, LoopLoc),
    findall(Module, unlinkable_chain(Loop, Module, _, _), UnlinkL),
    ( UnlinkL \= []
    ->( Issue = l(LoopLoc/Loop)-UnlinkL
      ; Issue = u(LinkLoc/Module)-ExLoc/Data,
        unlink_loop(Loop, Module, Left->M1, Right->M3),
        module_property(Module, file(File)),
        from_location(file(File, 1, _, _), LinkLoc),
        ( Data = M1:Left/l,
          Rel = [M1, Module]
        ; Data = M3:Right/r,
          Rel = [Module, M3]
        ),
        guess_loop_location(Rel, ExLoc)
      )
    ; module_pred_links(Loop, PredL, StrongL),
      ( maplist(=[], StrongL)
      ->Issue = s(LoopLoc/Loop)-PredL
      ; Issue = m(LoopLoc/Loop)-PredL
      )
    ).

guess_loop_location(Loop, Loc) :-
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
