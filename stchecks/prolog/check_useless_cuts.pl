/*  Part of Static Checks

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/stchecks
    Copyright (C): 2019, Process Design Center, Breda, The Netherlands.
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

:- module(check_useless_cuts, []).

:- use_module(library(lists)).
:- use_module(library(solution_sequences)).
% :- use_module(library(sequence_list)).
:- use_module(library(assertions)).
:- use_module(library(from_utils)).
:- use_module(library(neck)).
:- use_module(library(abstract_interpreter)).
:- use_module(library(gcu)).
:- use_module(library(countsols)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(safe_prolog_cut_to)).
:- use_module(library(checkable_predicate)).
:- use_module(library(checker)).
:- use_module(library(check), []).

:- multifile
    prolog:message//1,
    walk_body_hook/9,
    has_body_hook/2.

:- dynamic
    cut_info/3,
    inferred_det_db/3,
    shown_undefined/2,
    det_checking/1,
    det_checking/2,
    det_clause_db/2.

prolog:message(acheck(useless_cuts)) -->
    ['Check useless cuts',nl,
     '------------------',nl,
     'The predicates contain cuts that are actually not needed.', nl,
     'That could happen when the analysis determined that all', nl,
     'the calls before the cut and the clause itself where', nl,
     'deterministic or they have one of the next properties:', nl,
     'det, semidet, is_det or fails.  Note that in recursive', nl,
     'predicates the cut would look as needed, but it is not', nl,
     'since it can be removed or avoided via refactoring.', nl, nl].
prolog:message(acheck(useless_cuts, Issue)) -->
    issue_type_message(Issue).

issue_type_message(useless_cut(Loc, CI)-CutPosL) -->
    Loc,
    {length(CutPosL, N)},
    ['~q has ~w useless cut(s) (~q)'-[CI, N, CutPosL], nl].

checker:check(useless_cuts, Result, Options) :-
    check_useless_cuts(Options, Result).

check_useless_cuts(Options, Pairs) :-
    option_module_files(Options, MFileD),
    cuts_check(MFileD, Pairs),
    cleanup_useless_cuts.

cleanup_useless_cuts :-
    retractall(cut_info(_, _, _)),
    retractall(det_checking(_)),
    retractall(det_checking(_, _)),
    retractall(det_clause_db(_, _)),
    retractall(inferred_det_db(_, _, _)).

cuts_check(MFileD, Pairs) :-
    forall(current_det_check(MFileD),
           true),
    findall(warning-Issue,
            collect_issues(Issue, MFileD), Pairs).

collect_issues(useless_cut(Loc, CI)-CutPos, MFileD) :-
    retract(cut_info(From, RCutPos, unused)),
    ( nonvar(From),
      From = clause(Ref)
    ->nth_clause(M:H, I, Ref),
      functor(H, F, A),
      CI = M:F/A-I
    ; CI = unknown
    ),
    get_dict(M, MFileD, FileD),
    from_to_file(From, File),
    get_dict(File, FileD, _), % Avoid warnings from out there
    reverse(RCutPos, CutPos), % reverse is more human-readable
    from_location(From, Loc).

% 1. A cut is useless, if is located at the last clause, and the literals above
% are semidet

blocked(_, globprops).
blocked(_, opt_type(_, _, _)).

current_det_check(MFileD) :-
    order_by([asc(M:F/A)],
             ( get_dict(M, MFileD, FileD),
               current_predicate(M:F/A),
               functor(H, F, A),
               MH = M:H,
               % Start analyzing exported predicates, ancillary predicates will
               % be analyzed by det_check/3 if the argument instantiation can
               % not be determined, or by walk_lit/5 for specific
               % instantiations.
               predicate_property(MH, exported),
               \+ predicate_property(MH, imported_from(_)),
               \+ \+ ( catch(clause(MH, _, Ref), _, fail),
                       clause_property(Ref, file(File)),
                       get_dict(File, FileD, _)
                     ),
               \+ blocked(H, M)
             )),
    check_det(H, M, _).

clauses_accessible(MH) :-
    \+ is_built_in(MH),
    \+ predicate_property(MH, foreign).

check_det(H, M, Det) :-
    ( inferred_det_db(H, M, Det)
    ->true
    ; do_check_det(H, M, Det)
    ->assertz(inferred_det_db(H, M, Det))
    ; print_message(error,
                    format("unexpected failure of infer_det/3 or predef_det/3 ~q",
                           [M:H]))
    ).

do_check_det(H, M, Det) :-
    ( det_predef(Det, H, M)
    ->true
    ; with_det_checking_pr(
          H, M, catch(forall(walk_call(H, M, info), true),
                      Error,
                      print_message(
                          error,
                          Error))),
      infer_det(H, M, Det)
    ).

inferred_det(C, M, Det) :-
    functor(C, F, A),
    functor(H, F, A),
    predicate_property(M:H, implementation_module(I)),
    check_det(H, I, Det1),
    Det1 = Det.

valid_prop_asr(Lit, M, Asr) :-
    prop_asr(Lit, M, Status, Type, _, _, Asr),
    valid_status(Status),
    valid_type(Type).

valid_status(true).
valid_status(check).

valid_type(pred).
valid_type(prop).

valid_glob_asr(GL, A) :-
    member(G, GL),
    prop_asr(glob, globprops:G, _, A).

:- public
    det_props/2.

det_props(fails, [fails(_), failure(_)]).
det_props(isdet, [det(_), semidet(_), is_det(_), no_choicepoints(_)]).
det_props(nodet, [multi(_), non_det(_), nondet(_)]).

det_predef_asr(Det, H, M) :-
    det_props(Det, GL),
    neck,
    collect_valid_glob_asr(Det, GL, H, M).

collect_valid_glob_asr(Det, GL, H, M) :-
    member(Det, [fails, isdet]),
    neck,
    \+ \+ valid_prop_asr(H, M, _),
    forall(valid_prop_asr(H, M, A),
           valid_glob_asr(GL, A)).
collect_valid_glob_asr(Det, GL, H, M) :-
    member(Det, [nodet]),
    neck,
    valid_prop_asr(H, M, A),
    valid_glob_asr(GL, A).

undefined_found(C, M) :-
    ( \+ shown_undefined(C, M)
    ->assertz(shown_undefined(C, M)),
      functor(C, F, A),
      existence_error(procedure, F/A)
    ; true
    ).

%!  det_predef(-DetInfo, +Head, +Module) is multi.
%
%   Get Determinism information for a given predicate, that comes from
%   user-defined assertions (which has priority) or predicate properties. If the
%   predicate is being analyzed, assumes nondet, which is the value used when
%   there is no determinism information (yet). Be aware that only the first
%   solution makes sense.

det_predef(Det,   H, M) :- det_predef_asr(Det, H, M).
% TBD: put these hard-wired clauses in assertions:
det_predef(nodet, cd_invert(_, _, _, _, _), clpcd_inv).
det_predef(isdet, compare_d(_, _, _, _), clpcd_domain_ops).
det_predef(nodet, ineq(_, _, _, _, _), clpcd_ineq).
det_predef(isdet, ineq_one(_, _, _, _, _), clpcd_ineq).
det_predef(isdet, ineq_one_n_n_0(_, _), clpcd_ineq).
det_predef(isdet, ineq_one_n_p_0(_, _), clpcd_ineq).
det_predef(isdet, ineq_one_s_n_0(_, _), clpcd_ineq).
det_predef(isdet, ineq_one_s_p_0(_, _), clpcd_ineq).
det_predef(nodet, solve(_, _), clpcd_solve).
det_predef(isdet, reconsider(_, _), clpcd_solve).
det_predef(isdet, unconstrained(_, _, _, _, _), clpcd_solve).
det_predef(isdet, i18n_to_translate(_, _), i18n_support).
det_predef(isdet, add_linear_11(_, _, _, _), clpcd_store).
det_predef(isdet, add_linear_f1(_, _, _, _, _), clpcd_store).
det_predef(isdet, add_linear_ff(_, _, _, _, _, _), clpcd_store).
det_predef(nodet, solve_2nd_eq(_, _, _, _, _, _), clpcd_nf).
det_predef(nodet, H, M) :- blocked(H, M).
det_predef(nodet, H, M) :- det_checking(H, M).
det_predef(nodet, H, M) :- \+ clauses_accessible(M:H).
det_predef(fails, H, M) :-
    \+ predicate_property(M:H, defined),
    undefined_found(H, M).
det_predef(fails, H, M) :-
    \+ predicate_property(M:H, dynamic),
    \+ predicate_property(M:H, multifile),
    predicate_property(M:H, number_of_clauses(0 )).
det_predef(nodet, current_predicate(P), _) :- var(P).
det_predef(nodet, current_module(M), _) :- var(M).

infer_det(H, M, Det) :-
    member(Det1, [multi, isdet, fails]),
    \+ \+ ( current_clause(M:H, _, From),
            det_clause(From, Det1)
          ),
    !,
    Det = Det1.

add_cp.
add_cp :- fail.


cond_cp(CP1, CP2) :- CP1 \= CP2.
cond_cp(CP1, CP2) :- CP1 == CP2.

:- meta_predicate
    with_det_checking_pr(+, +, 0 ),
    with_det_checking_cl(+, +, 0 ).


%!  with_det_checking_pr(+Head, +Module, :Call).
%!  with_det_checking_cl(+Head, +Module, :Call).

%   with_det_checking ensures decidability by not analyzing if there is a
%   recursion that requires to analyze the same call again (abstraction step)

with_det_checking_pr(H, M, Call) :-
    ( det_checking(H, M)
    ->true
    ; functor(H, F, A),
      functor(P, F, A),
      setup_call_cleanup(
          assertz(det_checking(P, M), DCRef),
          ( Call,
            erase(DCRef)
          ),
          erase_nf(DCRef))
    ).

with_det_checking_cl(info, _,    Call) :- call(Call).
with_det_checking_cl(noop, From, Call) :-
    ( det_checking(From)
    ->add_cp
    ; setup_call_cleanup(
          assert_det_checking(From, Ref),
          ( call(Call),
            erase(Ref)
          ),
          erase_nf(Ref))
    ).

erase_nf(Ref) :- ignore(erase(Ref)).

assert_det_checking(From, Ref) :-
    assertz(det_checking(From), Ref).

:- meta_predicate current_clause(0, -, -).

current_clause(MH, Body, From) :-
    \+ predicate_property(MH, dynamic),
    \+ predicate_property(MH, multifile),
    !,
    match_head_body(MH, Body, From).
current_clause(MH, Body, From) :-
    % for dynamic and multifile, add a choicepoint at the end of the clause
    ( match_head_body(MH, Body, From)
    ; Body = user:true,
      From = [],
      add_cp
    ).

current_clause(MH, Body, From, CP1, CP2) :-
    prolog_current_choice(CP1),
    current_clause(MH, Body, From),
    prolog_current_choice(CP2).

:- meta_predicate call_with_location(0, +).

call_with_location(Call, From) :-
    catch(Call, Error, throw(at_location(From, Error))).

walk_call(H, M, CA) :-
    current_clause(M:H, Body, From, CP1, CP2),
    ( From = []
    ->add_det_clause(CA, From, CP1, CP2)
    ; Body = _:true
    ->true
    ; add_neg_clause(CA, From),
      call_with_location(
          with_det_checking_cl(
              CA, From,
              do_walk_body(Body, From, CA, CP1, CP2)),
          From)
    ),
    remove_new_cp(CA, CP2).

cut_to(CP) :- catch(safe_prolog_cut_to(CP), _, true).

remove_new_cp(noop, _).
remove_new_cp(info, CP) :- cut_to(CP).

do_walk_body(CM:Body, From, CA, CP1, CP2) :-
    % We can not cut CP1, since we are analyzing clause by clause, so if there
    % is a choice point at clause level, we insert a choicepoint with add_cp to
    % detect any cut in the clause
    prolog_current_choice(CP3),
    cond_cp(CP1, CP2),
    prolog_current_choice(CP4),
    walk_body(Body, CM, [], From, CA, CP3, CP4, _),
    prolog_current_choice(CP5),
    add_det_clause(CA, From, CP3, CP5).

add_cut_info(unused, LitPos, From) :-
    ( cut_info(From, LitPos, _)
    ->true
    ; assertz(cut_info(From, LitPos, unused))
    ).
add_cut_info(needed, LitPos, From) :-
    retractall(cut_info(From, LitPos, _)),
    assertz(cut_info(From, LitPos, needed)).

det_clause(From, Det) :-
    det_clause_db(From, Det1),
    !,
    Det = Det1.
det_clause(clause(Ref), Det) :-
    nth_clause(Pred, Idx, Ref),
    clause(_, Body, Ref),
    Body = true,
    !,
    ( predicate_property(Pred, number_of_clauses(Idx))
    ->Det = isdet
    ; Det = multi
    ).
det_clause(_, multi).

add_det_clause(info, From, CP2, CP3) :-
    ( CP2 == CP3
    ->DetInfo = isdet
    ; DetInfo = multi
    ),
    retractall(det_clause_db(From, _)),
    assertz(det_clause_db(From, DetInfo)).
add_det_clause(noop, _, _, _).

add_neg_clause(info, From) :-
    assertz(det_clause_db(From, fails)).
add_neg_clause(noop, _).

add_pos([], Pos, [Pos]).
add_pos([Pos1|LitPos1], Pos, LitPos) :-
    ( Pos1 = Pos
    ->LitPos = [Pos-2|LitPos1]
    ; Pos1 = Pos-N1
    ->succ(N1, N),
      LitPos = [Pos-N|LitPos1]
    ; LitPos = [Pos, Pos1|LitPos1]
    ).

disj_list((A;B), [A-MX|L]) :-
    nonvar(A),
    member(A-MX, [(_->_)-mutex, (_*->_)-notmx]),
    !,
    disj_list(B, L).
disj_list(A, [A-notmx]).

walk_body(V, M, _, _, _, _, CP, CP) :-
    ( var(V)
    ; var(M)
    ),
    !,
    add_cp.
/*
walk_body(A, _, LitPos, From, _, CP1, _, CP1) :-
    sequence_list(A, L, []),
    append(_, [!], L),
    !,
    walk_body_cut(LitPos, From, CP1).
*/
walk_body(!, _, LitPos, From, _, CP1, _, CP1) :-
    !,
    walk_body_cut(LitPos, From, CP1).
walk_body(M:A, _, LitPos, From, CA, CP1, CP2, CP) :-
    !,
    add_pos(LitPos, 2, LitPosA),
    walk_body(A, M, LitPosA, From, CA, CP1, CP2, CP).
walk_body((A, B), M, LitPos, From, CA, CP1, CP2, CP) :-
    !,
    add_pos(LitPos, 1, LitPosA),
    add_pos(LitPos, 2, LitPosB),
    walk_body(A, M, LitPosA, From, CA, CP1, CP2, CP3),
    walk_body(B, M, LitPosB, From, CA, CP1, CP3, CP).
walk_body((A; B), M, LitPos, From, CA, CP1, CP2, CP) :-
    !,
    ( disj_list((A; B), L),
      L = [_, _|_]
    ->prolog_current_choice(CP3),
             %s(FlagL, L, IsDet, MutEx
      SData = s([], L, fails, mutex),
      catch(
          ( forall(( nth1(Pos, L, E-MX),
                     call_cleanup(
                         ( add_pos(LitPos, Pos, LitPosL),
                           walk_body_if_branch(E, M, LitPosL, From, noop, SData, CP1, CP3)
                         ),
                         nb_setarg(4, SData, MX))),
                   ( arg(2, SData, L1),
                     greatest_common_unifier(L, L1, L2),
                     nb_setarg(2, SData, L2)
                 )),
            SData = s(FlagL, L, IsDet, _)
          ),
          top_reached,
          ( FlagL = [retain_cp],
            IsDet = multi
          )),
      ( member(retain_cp, FlagL) % CP1 should not be cutted
      ->CP = CP2
      ; cut_to(CP1),
        CP = CP1
      ),
      ( IsDet = multi
      ->add_cp
      ; true
      )
    ; ( prolog_current_choice(CP3),
        % add_cp will mark a cut as needed, since it can cut the internal
        % choicepoint of :/2
        add_cp,
        prolog_current_choice(CP4),
        add_pos(LitPos, 1, LitPosA),
        walk_body(A, M, LitPosA, From, CA, CP3, CP4, CP)
      ; add_pos(LitPos, 2, LitPosB),
        walk_body(B, M, LitPosB, From, CA, CP1, CP2, CP)
      )
    ).
walk_body((A->B), M, LitPos, From, CA, CP1, CP2, CP) :-
    !,
    ( prolog_current_choice(CP3),
      add_pos(LitPos, 1, LitPosA),
      walk_body(A, M, LitPosA, From, CA, CP3, CP2, CP4)
    ->add_pos(LitPos, 2, LitPosB),
      walk_body(B, M, LitPosB, From, CA, CP1, CP4, CP)
    ).
walk_body((A*->B), M, LitPos, From, CA, CP1, CP2, CP) :-
    !,
    ( prolog_current_choice(CP3),
      add_pos(LitPos, 1, LitPosA),
      walk_body(A, M, LitPosA, From, CA, CP3, CP2, CP4),
      add_pos(LitPos, 2, LitPosB),
      walk_body(B, M, LitPosB, From, CA, CP1, CP4, CP)
    ).
walk_body(call(A), M, LitPos, From, CA, _, CP2, CP) :-
    !,
    prolog_current_choice(CP3),
    add_pos(LitPos, 1, LitPosA),
    walk_body(A, M, LitPosA, From, CA, CP3, CP2, CP).
walk_body(\+ (A), M, LitPos, From, CA, _, CP2, CP2) :-
    !,
    \+ ( prolog_current_choice(CP),
         add_pos(LitPos, 1, LitPosA),
         walk_body(A, M, LitPosA, From, CA, CP, CP, _),
         fail
       ).
walk_body(true, _, _, _, _, _, CP, CP) :- !.
walk_body(fail, _, _, _, _, _, CP, _) :-
    !,
    cut_to(CP),
    fail.
walk_body(false, _, _, _, _, _, CP, _) :-
    !,
    cut_to(CP),
    fail.
walk_body(A=B, _, _, _, _, _, CP, CP) :-
    !,
    ( A = B
    ->true
    ; cut_to(CP),
      fail
    ).
walk_body(A\=B, _, _, _, _, _, CP, CP) :-
    !,
    ( A \= B
    ->true
    ; A \== B
    ->true
    ; A == B
    ->cut_to(CP),
      fail
    ).
walk_body(atom_concat(_, B, _), _, _, _, _, _, CP, CP) :-
    atomic(B),
    !.
walk_body(atom_concat(A, B, C), _, _, _, _, _, CP, CP) :-
    atomic(C),
    !,
    atom_concat(A, B, C).
walk_body(atom_length(A, B), _, _, _, _, _, CP, CP) :-
    atomic(A),
    !,
    atom_length(A, B).
walk_body(atom_length(_, _), _, _, _, _, _, CP, CP) :- !.
walk_body(nb_getval(A, B), _, _, _, _, _, CP, CP) :-
    ignore((nonvar(A), nb_current(A, B))).
walk_body(A, M, LitPos, From, CA, CP1, CP2, CP) :-
    predicate_property(M:A, implementation_module(I)),
    abstract_interpreter:replace_body_hook(A, I, B),
    !,
    walk_body(B, M, LitPos, From, CA, CP1, CP2, CP).
walk_body(C, M, LitPos, From, CA, CP1, CP2, CP3) :-
    predicate_property(M:C, implementation_module(I)),
    has_body_hook(C, I),
    !,
    walk_body_hook(C, I, M, LitPos, From, CA, CP1, CP2, CP3).
walk_body(A, M, _, From, _, _, CP, CP) :-
    abstract_interpreter:evaluable_body_hook(A, M, Condition),
    call(Condition),
    !,
    ( \+ \+ call_with_location(M:A, From)
    ->call(M:A)
    ; cut_to(CP),
      fail
    ).
walk_body(@(M:H, C), _, _, From, _, _, CP, CP) :-
    walk_lit(H, M, C, From, CP).
walk_body(H, M, _, From, _, _, CP, CP) :-
    walk_lit(H, M, M, From, CP).

walk_body_if_branch(C, M, LitPos, From, CA, SData, CP1, CP2) :-
    ( CP1 == CP2
    ->prolog_current_choice(CP3)
    ; prolog_current_choice(CP3),
      ( true
      ; arg(1, SData, FlagL),
        nb_setarg(1, SData, [retain_cp|FlagL]),
        fail
      )
    ),
    walk_body_if_branch_2(C, M, LitPos, From, CA, SData, CP3).

walk_body_if_branch_2(C, M, LitPos, From, CA, SData, CP3) :-
    prolog_current_choice(CP4),
    countsols(N, walk_body(C, M, LitPos, From, CA, CP3, CP4, _)),
    prolog_current_choice(CP5),
    arg(3, SData, OldDet),
    arg(4, SData, MX),
    ( CP4 == CP5
    ->IsDet = isdet
    ; IsDet = multi
    ),
    ( N >= 10
      % prevent performance issues by limiting the solutions of a goal to 10
    ->print_message(warning, format("In ~w, solutions (~w) >= 10.", M:C, N)),
      throw(top_reached)
    ; true
    ),
    once(mutex_prev(MX, OldDet, IsDet, NewDet)),
    nb_setarg(3, SData, NewDet).

mutex_prev(notmx, isdet, isdet, multi).
mutex_prev(mutex, isdet, isdet, isdet).
mutex_prev(_,     fails, isdet, isdet).
mutex_prev(_,     _,     multi, multi).
mutex_prev(_,     multi, _,     multi).

walk_body_cut(LitPos, From, CP1) :-
    prolog_current_choice(CP),
    ( CP1 \= CP
    ->Info = needed,
      cut_to(CP1)
    ; Info = unused
    ),
    add_cut_info(Info, LitPos, From).

:- public
    curr_wrapper/3.

curr_wrapper(start_tabling(_, C), '$tabling', C).

has_body_hook(W, M) :-
    curr_wrapper(W, M, _),
    neck.

walk_body_hook(W, I, M, LitPos, From, CA, CP1, CP2, CP3) :-
    curr_wrapper(W, I, A),
    neck,
    walk_body(call(A), M, LitPos, From, CA, CP1, CP2, CP3).

walk_lit(V, M, _, _, _) :-
    ( var(V)
    ; var(M)
    ),
    !,
    add_cp.
walk_lit(H, M, CM, From, CP) :-
    ( predicate_property(M:H, meta_predicate(Meta))
    ->qualify_meta_goal(CM:H, Meta, C)
    ; C = H
    ),
    catch(inferred_det(C, M, Det),
          Error,
          ( print_message(error, at_location(From, Error)),
            Det = fails
          )),
    ( Det = nodet
      % We have to check for nodet, instead of multi, since all predicates that
      % don't have det properties inferred yet are marked as multi.
    ->add_cp
    ; % We can check multi only if all arguments of C are independent variables,
      % meaning that we can reuse the result of the determinism analysis:
      functor(C, F, A),
      functor(P, F, A),
      C =@= P,
      Det = multi
    ->add_cp
    ; Det = isdet
    ->true
    ; Det = fails
    ->cut_to(CP),
      fail
    ; catch(findall(-, current_clause(M:C, _, _), ClauseL),
            _,
            AddCP = true),
      ( ClauseL = []
      ->cut_to(CP),
        fail
      ; AddCP == true
      ->add_cp
      ; walk_call(C, M, noop)
      )
    ).
