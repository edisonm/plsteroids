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
     'refactoring or module merge to decouple the involved code.', nl,
     'If the list of strongly connected predicates is empty, it means', nl,
     'that is possible to resolve the loop involving more than 2 modules', nl,
     'and it will be reported as a complex module loop.', nl, nl].

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
    ml_msg_alternative_path(M2, 'used by', Loc1, M1, PL1, L1),
    ml_msg_alternative_path(M2, 'uses',    Loc3, M3, PL3, L3).

ml_msg_alternative_path(M2, Label, Loc, M, PL, L) -->
    ['\t'|Loc], ["~w ~w ~w: ~w"-[M2, Label, M, L]],
    ( {PL \= []}
    ->[".  Alternative paths present: ~w"-[PL]]
    ; []
    ),
    [nl].

checker:check(module_loops, Pairs, Options) :-
    collect_calls_to(Options, _),
    update_depends_of,
    module_loops(Loops, Options),
    findall(Pair, loops_pairs(Loops, Pair), Pairs).

loops_pairs(Loops, warning-Issue) :-
    member(Loop, Loops),
    guess_link_location(Loop, LoopLoc),
    findall([M2, Loc1, M1, PL1, L1, Loc3, M3, PL3, L3],
            ( unlinkable_chain(Loop, M1, M2, M3),
              findall([I|P],
                      ( loads_db(M1, I, _, 1),
                        loads_db(I, M2, P, _),
                        \+ memberchk(M1, P)
                      ), PL1),
              findall([I|P],
                      ( loads_db(M2, I, _, 1),
                        loads_db(I, M3, P, _),
                        \+ memberchk(M2, P)
                      ), PL3),
              mod_used_by_preds(M2, M1, L1),
              module_uses_preds(M2, M3, L3),
              guess_link_location([M1, M2], Loc1),
              guess_link_location([M2, M3], Loc3)
            ), UnlinkL),
    ( UnlinkL \= []
    ->Issue = l(LoopLoc/Loop)-UnlinkL
    ; module_pred_links(Loop, PredL, StrongL),
      ( maplist(=[], StrongL)
      ->Issue = s(LoopLoc/Loop)-PredL
      ; Issue = m(LoopLoc/Loop)-PredL
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
