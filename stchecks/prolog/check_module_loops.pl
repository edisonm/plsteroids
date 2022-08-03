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
:- use_module(library(solution_sequences)).
:- use_module(library(calls_to)).
:- use_module(library(checker)).
:- use_module(library(module_loops)).
:- use_module(library(module_links)).
:- use_module(library(module_uses)).
:- use_module(library(extra_location)).
:- use_module(library(location_utils)).

:- multifile
        prolog:message//1.

prolog:message(acheck(module_loops)) -->
    ['Module loops', nl,
     '------------', nl,
     'Module loops could potentially lead to Demeter\'s law violations', nl, nl].

prolog:message(acheck(module_loops, Issue)) -->
    module_loops_message_type(Issue).

module_loops_message_type(loop(Loc/Loop)-[UnlinkL]) -->
    Loc,
    ['Module loop found ~w, but can be broken at ~w'-[Loop, UnlinkL], nl].
module_loops_message_type(unlink(Loc/M)-IssueL) -->
    Loc,
    ['Module ~w can be splitted to decouple indirectly linked modules'-[M], nl],
    foldl(module_loops_unlink_message(M), IssueL).

module_loops_unlink_message(M, [LocL/M1:L1, LocR/M3:L3]) -->
    ['\t'|LocL], ['From ~w used by ~w: ~w'-[M, M1, L1], nl],
    ['\t'|LocR], ['From ~w uses ~w: ~w'-[M, M3, L3], nl].

checker:check(module_loops, Pairs, Options) :-
    collect_calls_to(Options, _),
    update_depends_of(_, _),
    collect_module_uses(_, _),
    module_loops(Loops, Options),
    maplist(normalize_loop, Loops, Norms),
    sort(Norms, Sorted),
    findall(Pair, loops_pairs(Sorted, Pair), Pairs).

normalize_loop(Loop, Norm) :-
    once(order_by([asc(Norm)],
                  ( append(Left, Right, Loop),
                    append(Right, Left, Norm)
                  ))).

loops_pairs(Loops, warning-Issue) :-
    member(Loop, Loops),
    ( findall(Module, unlink_loop(Loop, Module), UnlinkL),
      Issue = loop(Loc/Loop)-UnlinkL,
      guess_loop_location(Loop, Loc)
    ; unlink_loop(Loop, Module, Left->M1, Right->M3),
      Issue = unlink(Loc/Module)-[LocL/M1:Left, LocR/M3:Right],
      guess_loop_location([M1, Module], LocL),
      guess_loop_location([Module, M3], LocR),
      module_property(Module, file(File)),
      from_location(file(File, 1, _, _), Loc)
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
