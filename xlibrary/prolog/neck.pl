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
                 neckis/0,
                 neckis/2]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(list_sequence)).
:- use_module(library(choicepoints)).
:- use_module(library(statistics)).
:- use_module(library(ordsets)).
:- use_module(library(solution_sequences)).
:- reexport(library(compound_expand)).
:- reexport(library(checkct)).
:- use_module(library(compound_expand)).
:- reexport(library(checkct)).

:- multifile file_clause:head_calls_hook/5.

%!  neck        is det.
%!  necki       is det.
%!  necks       is det.
%!  neckis      is det.
%!  neck( L, L) is det.
%!  necki(L, L) is det.
%!  necks(L, L) is det.
%!  neckis(L, L) is det.
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

%   neckis is a combination of inlined and silent.

%   these predicates can also be used in declarations, although in that case, no
%   warnings  will  be   shown  about  run-time  parts   being  executed,  since
%   declarations are executed at compile-time.

neck.

neck --> [].

necki.

necki --> [].

necks.

necks --> [].

neckis.

neckis --> [].

:- thread_local
    head_calls_hook_db/5.

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

assign_value(A, V) -->
    ( {var(V)}
    ->{A=V}
    ; [A=V]
    ).

neck_prefix('__aux_neck_').

call_checks(File, Line, Call, HasCP) :-
    has_choicepoints(call_checkct(Call, File, Line, []), nb_setarg(1, HasCP, no)).

term_expansion_hb(File, Line, Head, Body1, NeckBody, Pattern, ClauseL) :-
    \+ ( nonvar(Head),
         current_prolog_flag(xref, true)
       ),
    '$current_source_module'(M),
    once(( current_seq_lit(Body1, Neck, Static, Right),
           memberchk(Neck, [neck, neck(X, X), necki, necki(X, X),
                            necks, necks(X, X), neckis, neckis(X, X)])
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
    HasCP = hascp(yes),
    term_variables(Head-Right, HNVarU),
    term_variables(Expanded, ExVarU),
    sort(HNVarU, HNVarL),
    sort(ExVarU, ExVarL),
    ord_intersection(ExVarL, HNVarL, AssignedL),
    ( memberchk(Neck, [neck, neck(_, _), necks, necks(_, _)]),
      Head \== '<declaration>',
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
      neck_prefix(NeckPrefix),
      format(atom(FNB), '~w~w:~w', [NeckPrefix, M, Hash]),
      SepHead =.. [FNB|ArgNB],
      conj(LRight, SepHead, NeckBody),
      strip_module(M:Head, MH, Pred),
      freeze(Pred, assertz(head_calls_hook_db(Pred, MH, Expanded, File, Line), Ref)),
      findall(t(Pattern, Head), call_checks(File, Line, Expanded, HasCP), ClausePIL),
      ( '$get_predicate_attribute'(M:SepHead, defined, 1),
        '$get_predicate_attribute'(M:SepHead, number_of_clauses, _)
      ->true
      ; ClausePIL \= [_]
      )
    ->RTHead = SepHead,
      phrase(( findall((:- discontiguous IM:F/A),
                       distinct(IM:F/A,
                                ( member(t(_, H), ClausePIL),
                                  H \== '<declaration>',
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
      strip_module(M:Head, MH, Pred),
      freeze(Pred, assertz(head_calls_hook_db(Pred, MH, Expanded, File, Line), Ref)),
      findall(t(Pattern, Head), call_checks(File, Line, Expanded, HasCP), ClausePIL),
      RTHead = Head,
      ClauseL1 = []
    ),
    ( Head == '<declaration>'
    ->true
    ; HasCP = hascp(yes)
    ->true
    % Since this is a critical warning, we prevent app programmers to be able
    % to disable it, in any case there is always a possibility to refactorize
    % the code to prevent this warning --EMM
    % ; memberchk(Neck, [necks, necks(_, _), neckis, neckis(_, _)])
    % ->true
    ; ClausePIL = [t(_, MHead)],
      strip_module(Head,  _, Head1),
      compound(Head1),
      strip_module(MHead, _, Head2),
      arg(1, Head1, Arg1),
      arg(1, Head2, Arg2),
      var(Arg1),
      nonvar(Arg2)
    ->true
    ; ClausePIL = [t(ClausePI, _)],
      % Compare performance with simple unification via =/2 to see if neck is
      % improving the performance or not. But note that this will compare
      % interpreted prolog, not optimized/compiled code:
      findall(T1, call_time(Expanded, T1), [T1]),
      InfCurrent = T1.inferences,
      copy_term(Pattern-AssignedL, ClausePI-ValuesL),
      foldl(assign_value, AssignedL, ValuesL, AVL, []),
      list_sequence(AVL, Sequence),
      findall(T2, call_time(Sequence, T2), [T2]),
      InfOptimal = T2.inferences,
      InfOptimal >= InfCurrent
    ->warning_nocp(File, Line, M, Head, InfOptimal >= InfCurrent),
      erase(Ref),
      fail
    ; true
    ),
    phrase(( findall(Clause, member(t(Clause, _), ClausePIL)),
             findall(Clause,
                     ( \+ memberchk(Neck, [necks, necks(_, _), neckis, neckis(_, _)]),
                       Head \== '<declaration>',
                       SepBody \= true,
                       distinct(Clause, st_body(Head, M, RTHead, ClausePIL, Clause))
                     ))
           ), ClauseL, ClauseL1).

term_expansion_hb(Head, Body1, NeckBody, Pattern, ClauseL) :-
    source_location(File, Line),
    term_expansion_hb(File, Line, Head, Body1, NeckBody, Pattern, ClauseL).

st_body(Head, M, RTHead, ClausePIL, Clause) :-
    member(t(_, Head), ClausePIL),
    strip_module(M:RTHead, RTM, RTPred),
    functor(RTPred, RTF, RTA),
    member(Clause, [(:- discontiguous RTM:RTF/RTA) % silent random warnings
                    %(:- multifile RTM:RTF/RTA) % silent audit warnings
                   ]).

warning_nocp(File, Line, M, H, Term) :-
    print_message(
        warning,
        at_location(
            file(File, Line, -1, _),
            format("Ignored neck on ~w, since it could cause performance degradation (~w)", [M:H, Term]))).

term_expansion((Head :- Body), ClauseL) :-
    term_expansion_hb(Head, Body, NB, (Head :- NB), ClauseL).
term_expansion((Head --> Body), ClauseL) :-
    current_seq_lit(Body, Neck, _, _),
    memberchk(Neck, [neck, necki, necks, neckis]),
    dcg_translate_rule((Head --> Body), _, (H :- B), _),
    term_expansion_hb(H, B, NB, (H :- NB), ClauseL).
term_expansion((:- Body), ClauseL) :-
    term_expansion_hb('<declaration>', Body, NB, (:- NB), ClauseL).
term_expansion(end_of_file, ClauseL) :-
    in_module_file,
    findall(file_clause:head_calls_hook(Head, M, Body, File, Line),
            retract(head_calls_hook_db(Head, M, Body, File, Line)),
            ClauseL, [end_of_file]).
