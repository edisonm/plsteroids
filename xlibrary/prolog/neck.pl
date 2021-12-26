/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
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

:- module(neck, [neck/0,
                 neck/2,
                 necki/0,
                 necki/2,
                 necks/0,
                 necks/2,
                 rtc_warning/3]).

:- use_module(library(lists)).
:- use_module(library(ordsets)).
:- use_module(library(solution_sequences)).
:- reexport(library(compound_expand)).

%!  neck        is det.
%!  necki       is det.
%!  necks       is det.
%!  neck( L, L) is det.
%!  necki(L, L) is det.
%!  necks(L, L) is det.
%
%   Establish that everything  above it should be evaluated at  compile time, be
%   careful since  such part  can only contain  predicates already  defined.  In
%   case  of non-determinism,  several clauses  would be  generated.  This  is a
%   practical way to  generate automatic clauses with a  proper instantiation of
%   the head. If neck can not be expanded, it will succeed without side effects.

%   neck is  used if  you want  to put the  body in  a separated  predicate, and
%   consider it the  run-time only part of  it, meaning that you can  not use it
%   until the compilation of the module has finished.

%   necki (i=inlined) is  used if you don't want to  create ancillary predicates
%   for the body, but rather have the body inlined.

%   necks (s=silent, or static) will not  warn you if the non-expanded parts are
%   called at compile-time.

%   these predicates can also be used in declarations, although in that case, no
%   warnings  will  be   shown  about  run-time  parts   being  executed,  since
%   declarations are executed at compile-time.

neck.

neck --> [].

necki.

necki --> [].

necks.

necks --> [].

current_seq_lit(Seq, Lit, Left, Right) :-
    current_seq_lit(Seq, Lit, true, Left, true, Right).

conj(T, C, C) :- T == true.
conj(C, T, C) :- T == true.
conj(A, B, (A, B)).

current_seq_lit(S, _, _, _, _, _) :-
    var(S),
    !,
    fail.
current_seq_lit(S, S, L, L, R, R).
current_seq_lit((H, T), S, L1, L, R1, R) :-
    ( once(conj(T, R1, R2)),
      current_seq_lit(H, S, L1, L, R2, R)
    ; once(conj(L1, H, L2)),
      current_seq_lit(T, S, L2, L, R1, R)
    ).

:- thread_local '$neck_body'/3.

term_expansion_hb(Head, Body1, NeckBody, Pattern, ClauseL) :-
    '$current_source_module'(M),
    once(( current_seq_lit(Body1, Neck, Static, Right),
           memberchk(Neck, [neck, neck(X, X), necki, necki(X, X), necks, necks(X, X)])
         )),
    once(( current_seq_lit(Right, !, LRight, SepBody),
           \+ current_seq_lit(SepBody, !, _, _)
           % We can not move the part above a cut to a separate clause
         ; LRight = true,
           SepBody = Right
         )),
    term_variables(Head, HVars),
    '$expand':mark_vars_non_fresh(HVars),
    expand_goal(M:Static, Expanded),
    ( memberchk(Neck, [neck, neck(_, _), necks, necks(_, _)]),
      Head \== '$decl',
      nonvar(SepBody),
      member(SepBody, [(_, _), (_;_), (_->_), \+ _]),
      expand_goal(M:SepBody, M:ExpBody),
      ExpBody \= true,
      term_variables(t(Head, Expanded, LRight), VarHU),
      '$expand':remove_var_attr(VarHU, '$var_info'),
      sort(VarHU, VarHL),
      term_variables(ExpBody, VarBU),
      sort(VarBU, VarBL),
      ord_intersection(VarHL, VarBL, ArgNB),
      variant_sha1(ArgNB-ExpBody, Hash),
      format(atom(FNB), '__aux_neck_~w:~w', [M, Hash]),
      SepHead =.. [FNB|ArgNB],
      conj(LRight, SepHead, NeckBody),
      findall(Pattern-Head, Expanded, ClausePIL),
      ( '$get_predicate_attribute'(M:SepHead, defined, 1),
        '$get_predicate_attribute'(M:SepHead, number_of_clauses, _)
      ->true
      ; ClausePIL \= [_]
      )
    ->RTHead = SepHead,
      phrase(( findall((:- discontiguous IM:F/A),
                       distinct(IM:F/A,
                                ( member(_-H, ClausePIL),
                                  H \== '$decl',
                                  strip_module(M:H, IM, P),
                                  functor(P, F, A)
                                ))),
               ( { '$get_predicate_attribute'(M:SepHead, defined, 1),
                   '$get_predicate_attribute'(M:SepHead, number_of_clauses, _)
                 }
               ->[]
               ; [(SepHead :- ExpBody)]
               )
             ), ClauseL1)
    ; expand_goal(M:Right, M:NeckBody),
      findall(Pattern-Head, Expanded, ClausePIL),
      RTHead = Head,
      ClauseL1 = []
    ),
    phrase(( findall(Clause, member(Clause-_, ClausePIL)),
             findall(Clause,
                     ( \+ memberchk(Neck, [necks, necks(_, _)]),
                       Head \== '$decl',
                       SepBody \= true,
                       distinct(Clause, st_body(Head, M, RTHead, ClausePIL, Clause))
                     ))
           ), ClauseL, ClauseL1).

st_body(Head, M, RTHead, ClausePIL, Clause) :-
    member(_-Head, ClausePIL),
    strip_module(M:Head, IM, Pred),
    strip_module(M:RTHead, RTM, RTPred),
    functor(RTPred, RTF, RTA),
    warning_body(IM, Pred, WB),
    (RTM = M->Body=WB; Body=M:WB),
    assertz('$neck_body'(M, RTM:RTPred, Body)),
    member(Clause, [(:- discontiguous RTM:RTF/RTA), % silent random warnings
                    (:- multifile RTM:RTF/RTA), % silent audit warnings
                    (:- dynamic RTM:RTF/RTA), (RTM:RTPred :- WB)]).

warning_body(M, H, rtc_warning(M, H, file(File, Line, -1, _))) :-
    source_location(File, Line).

%!  rtc_warning(+Module, +Head, +Location).
%
%   Shows a warning about a run-time call during compile-time and fails.

rtc_warning(M, H, Loc) :-
    source_location(File, Line),
    print_message(
        warning,
        at_location(
            file(File, Line, -1, _),
            at_location(
                Loc,
                format("Attempt to call run-time part of ~w at compile-time", [M:H])))),
    fail.

term_expansion((Head :- Body), ClauseL) :-
    term_expansion_hb(Head, Body, NB, (Head :- NB), ClauseL).
term_expansion((Head --> Body), ClauseL) :-
    current_seq_lit(Body, Neck, _, _),
    memberchk(Neck, [neck, necki, necks]),
    dcg_translate_rule((Head --> Body), _, (H :- B), _),
    term_expansion_hb(H, B, NB, (H :- NB), ClauseL).
term_expansion((:- Body), ClauseL) :-
    term_expansion_hb('$decl', Body, NB, (:- NB), ClauseL).
term_expansion(end_of_file, end_of_file) :-
    '$current_source_module'(CM),
    module_property(CM, file(File)),
    prolog_load_context(source, File),
    forall(( '$neck_body'(CM, Head, Body),
             clause(Head, Body, Ref)
           ),
           erase(Ref)),
    findall(IM:F/A,
            ( retract('$neck_body'(CM, Head, _)),
              strip_module(CM:Head, IM, Pred),
              functor(Pred, F, A)
            ), PIU),
    sort(PIU, PIL),
    compile_predicates(PIL).
