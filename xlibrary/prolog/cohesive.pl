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
          [ cohesive_module/4,
            cohesive_module_rt/6,
            freeze_cohesive_module_rt/6,
            scope_t/1,
            call_cm/3,
            call_cm/5,
            '$cohesive'/2
          ]).

:- use_module(library(apply)).
:- use_module(library(extend_args)).
:- use_module(library(normalize_head)).
:- use_module(library(option)).
:- use_module(library(sequence_list)).
:- use_module(library(call_ref)).
:- reexport(library(cohesive_op)).
:- reexport(library(compound_expand)).
:- init_expansors.

/* <module> Cohesive predicates

   This module provides support for cohesive predicates, those are like
   multifiles, but in order to use them, we need to import the predicates that
   define their clauses.  If two or more modules are imported, they are added
   up.  This provides certain level of encapsulation, but at the same time
   allows extensibility.  It also pays attention to reexported modules so that
   clauses in reexported modules of cohesive predicates become available in the
   importing module.

@author Edison Mera

*/

:- multifile
    '$cohesive'/2.

:- meta_predicate
        call_cm(0, +, -),
        call_cm(0, +, ?, -, -).

:- public freeze_cohesive_module_rt/6.

aux_cohesive_module(M, F, A, CohM, CohesiveModule) :-
    format(atom(CT), '__aux_cohm_~w:~w/~w', [M, F, A]),
    CohesiveModule =.. [CT, CohM].

aux_cohesive_pred(H, CohM, Scope, HExt) :-
    H =.. [F|Args],
    atom_concat('__aux_cohp_', F, FExt),
    HExt =.. [FExt, CohM, Scope|Args].
    % extend_args('__aux_cohp_', H, [CohM, Scope], HExt).

aux_cohesive_wrap(H, CM, CohM, HWrp) :-
    extend_args('__aux_cohw_', H, [CM, CohM], HWrp).

/* Note that if cohesive_module_rt/6 is called from the wrong context you will
 * get a run-time error since CheckCohM will not be defined, therefore you don't
 * need to implement a run-time check here, just let the predicate fail --EMM
*/

call_check_cohesive_module(H, Context, M, CohM, CheckCohM) :-
    ( % First, try with fast precompiled checker
      '$defined_predicate'(Context:CheckCohM)
    ->Context:CheckCohM
    ; % Second, use the slower alternative, it works at compile time
      '$defined_predicate'(Context:H),
      cohesive_module(H, Context, M, CohM)
    ).

cohesive_module_rt(_, user, _, _, _, _) :- !.
cohesive_module_rt(_, _, _, _, spublic, _).
cohesive_module_rt(H, Context, M, CohM, sexport, CheckCohM) :-
    call_check_cohesive_module(H, Context, M, CohM, CheckCohM).
cohesive_module_rt(_, C, _, C, sprivat, _).

cohesive_pred_pi(CM, PI) -->
    { normalize_head(CM:PI, M:H),
      aux_cohesive_pred(H, CohM, Scope, HExt),
      functor(H, F, A),
      aux_cohesive_module(M, F, A, CohM, CheckCohM),
      aux_cohesive_wrap(H, Context, CohM, HWrp),
      functor(HExt, FExt, AExt)
    },
    [ cohesive:'$cohesive'(H, M),
      (:- module_transparent M:F/A),
      (:- multifile M:FExt/AExt)
    ],
    ( {'$predicate_property'((discontiguous), M:H)}
    ->[(:- discontiguous M:FExt/AExt)]
    ; []
    ),
    [ ( H :- context_module(Context),
             call(CM:HWrp)
      ),
      ( HWrp :-
            freeze_cohesive_module_rt(H, Context, M, CohM, Scope, CheckCohM),
            HExt
      )
    ].

freeze_cohesive_module_rt(H, Context, M, CohM, Scope, CheckCohM) :-
    ignore(( Context \= user,
             % if called in the user context, asume all (equivalent to multifile)
             freeze(CohM, freeze(Scope, once(cohesive_module_rt(H, Context, M, CohM, Scope, CheckCohM))))
           )).

%!  call_cm(:Goal, +Context, -CohesiveModule) is multi.

%   Calls Goal and returns the module where the current clause was implemented from.

call_cm(Goal, Context, CohM, HWrp, IM) :-
    strip_module(Goal, _, Head),
    predicate_property(Goal, implementation_module(IM)),
    aux_cohesive_wrap(Head, Context, CohM, HWrp).

call_cm(Goal, Context, CohM) :-
    call_cm(Goal, Context, CohM, HWrp, IM),
    IM:HWrp.

coh_head_expansion(Scope, Head, IM:HeadExt) :-
    prolog_load_context(module, CM),
    predicate_property(CM:Head, implementation_module(IM)),
    '$cohesive'(Head, IM),
    % scope_module(Scope, CM, VM),
    aux_cohesive_pred(Head, CM, Scope, HeadExt).

% sprivat: can not be used externally
% sexport: needs use_module to use it (default)
% spublic: available to all (like user context)

scope_t(spublic).
scope_t(sexport).
scope_t(sprivat).

check_cohm_clause(Context, H, IM, Clause) :-
    predicate_property(Context:H, implementation_module(IM)),
    functor(H, F, A),
    aux_cohesive_module(IM, F, A, CohM, CheckCohM),
    ( % Note: CheckCohM must not be multifile, otherwise it will
      % remain defined on recompilation and the compilation result
      % will not be correct --EMM
      Clause = Context:CheckCohM,
      aux_cohesive_pred(H, CohM, _Scope, HExt),
      cohesive_module(H, Context, IM, CohM),
      ( CohM \= Context
      ->once(call_ref(IM:HExt, _, _))
      ; true
      )
    ).

check_cohm_clauses(Context, ClauseL) :-
    findall(Clause,
            ( '$cohesive'(H, IM),
              check_cohm_clause(Context, H, IM, Clause)
            ), ClauseL, [end_of_file]).

term_expansion(end_of_file, ClauseL) :-
    prolog_load_context(module, Context),
    module_property(Context, file(File)),
    prolog_load_context(source, File),
    check_cohm_clauses(Context, ClauseL).
term_expansion((:- cohesive_pred PIs), ClauseL) :-
    prolog_load_context(module, CM),
    sequence_list(PIs, PIL, []),
    foldl(cohesive_pred_pi(CM), PIL, ClauseL, []).
term_expansion(Scope::Head :- Body, HeadExt :- Body) :-
    scope_t(Scope),
    coh_head_expansion(Scope, Head, HeadExt).
term_expansion(Scope::Head, HeadExt) :-
    scope_t(Scope),
    coh_head_expansion(Scope, Head, HeadExt).
term_expansion((::Head :- Body), (HeadExt :- Body)) :-
    coh_head_expansion(sexport, Head, HeadExt).
term_expansion(::Head, HeadExt) :-
    coh_head_expansion(sexport, Head, HeadExt).
term_expansion((Head :- Body), (HeadExt :- Body)) :-
    coh_head_expansion(sprivat, Head, HeadExt).
term_expansion(Head, HeadExt) :-
    coh_head_expansion(sprivat, Head, HeadExt).

:- thread_local
    cm_db/2.

%!  cohesive_module(+H, +Context, +IM, -CohM) is multi.

cohesive_module(H, Context, IM, CohM) :-
    setup_call_cleanup(
        prolog_current_choice(CP),
        cohesive_module_1st(CP, H, Context, IM, CohM),
        retractall(cm_db(_, CP))).

cohesive_module_1st(CP, _, Context, _, Context) :-
    assertz(cm_db(Context, CP)).
cohesive_module_1st(CP, H, Context, IM, CM) :-
    '$load_context_module'(File, Context, _),
    module_property(M, file(File)),
    \+ cm_db(M, CP),
    predicate_property(M:H, implementation_module(IM)),
    cohesive_module_rec(CP, H, M, IM, CM).

cohesive_module_rec(CP, _, Context, _, Context) :-
    assertz(cm_db(Context, CP)).
cohesive_module_rec(CP, H, C, IM, CM) :-
    '$load_context_module'(File, C, Options),
    option(reexport(true), Options),
    module_property(M, file(File)),
    \+ cm_db(M, CP),
    predicate_property(M:H, implementation_module(IM)),
    cohesive_module_rec(CP, H, M, IM, CM).
