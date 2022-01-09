/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2022, Process Design Center, Breda, The Netherlands.
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

:- module(cohesive,
          [ op(1150, fx, cohesive_pred)
          ]).

:- use_module(library(sequence_list)).
:- use_module(library(extend_args)).
:- use_module(library(normalize_head)).
:- use_module(library(option)).
:- reexport(library(compound_expand)).

/* <module> Cohesive predicates

   This module provides support for cohesive predicates, those are like
   multifiles, but in order to use them, we need to import the predicates that
   defines their clauses.  If two or more modules are imported, they are added
   up.  This provides certain level of encapsulation, but at the same time
   allows extensibility. TBD: this is very experimental, it should be tested in
   the battlefield to see if is usable --EMM

@author Edison Mera

*/

:- multifile
    '$cohesive'/2.

:- thread_local
    cm_db/1.

aux_cohesive(M, F, A, CohM, CohesiveModule) :-
    format(atom(CT), '__aux_cohesive_~w:~w/~w', [M, F, A]),
    CohesiveModule =.. [CT, CohM].

cohesive_pred_pi(CM, PI) -->
    { normalize_head(CM:PI, M:H),
      extend_args(H, [CohM], HExt),
      functor(H, F, A),
      succ(A, AExt),
      aux_cohesive(M, F, A, CohM, CohesiveModule)
    },
    [ cohesive:'$cohesive'(H, M),
      (:- module_transparent M:F/A),
      (:- multifile M:F/AExt),
      (H :- context_module(Context),
            freeze(CohM, Context:CohesiveModule),
            % Origintal code to resolve this dynamically, but is slow:
            % freeze(CohM, once(cohesive_module(H, Context, M, CohM))),
            HExt)
    ].

term_expansion((:- cohesive_pred PIs), ClauseL) :-
    prolog_load_context(module, CM),
    sequence_list(PIs, PIL, []),
    foldl(cohesive_pred_pi(CM), PIL, ClauseL, []).

term_expansion((Head :- Body), (IM:HeadExt :- Body)) :-
    prolog_load_context(module, CM),
    predicate_property(CM:Head, imported_from(IM)),
    '$cohesive'(Head, IM),
    extend_args(Head, [CM], HeadExt).
term_expansion(Head, IM:HeadExt) :-
    prolog_load_context(module, CM),
    predicate_property(CM:Head, imported_from(IM)),
    '$cohesive'(Head, IM),
    extend_args(Head, [CM], HeadExt).
term_expansion(end_of_file, ClauseL) :-
    prolog_load_context(module, Context),
    findall(Context:CohesiveModule,
            ( '$cohesive'(H, IM),
              predicate_property(Context:H, imported_from(IM)),
              functor(H, F, A),
              call_cleanup(cohesive_module(H, Context, IM, CM),
                           retractall(cm_db(_))),
              aux_cohesive(IM, F, A, CM, CohesiveModule)
            ), ClauseL, [end_of_file]).

cohesive_module(_, Context, _, Context) :-
    assertz(cm_db(Context)).
cohesive_module(H, Context, IM, CM) :-
    '$load_context_module'(File, Context, _),
    module_property(M, file(File)),
    \+ cm_db(M),
    predicate_property(M:H, imported_from(IM)),
    cohesive_module_rec(H, M, IM, CM).

cohesive_module_rec(_, Context, _, Context) :-
    assertz(cm_db(Context)).
cohesive_module_rec(H, C, IM, CM) :-
    '$load_context_module'(File, C, Options),
    option(reexport(true), Options),
    module_property(M, file(File)),
    \+ cm_db(M),
    predicate_property(M:H, imported_from(IM)),
    cohesive_module_rec(H, M, IM, CM).
