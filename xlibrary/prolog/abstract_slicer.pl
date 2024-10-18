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

:- module(abstract_slicer,
          [abstract_slice/3,
           abstract_slice/4,
           apply_mode/5,
           slicer_abstraction/7]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pure)).
:- use_module(library(abstract_interpreter)).
:- use_module(library(terms_share)).

:- meta_predicate
    abstract_slice(0,+,?),
    abstract_slice(0,+,?,-),
    slicer_abstraction(+,+,+,0,?, ?,?).

/** <module> Abstract slicer

Implements the next  abstract domain: find possible matches  of output arguments
of the given predicate.

*/

abstract_slice(M:Head, Mode, OptL) :-
    abstract_slice(M:Head, Mode, OptL, _).

%!  abstract_slice(:Head, +Mode:list, +Options, +State) is multi.
%
%   Returns  on  backtracking, the  possible  instances  in  the Head  of  those
%   argument positions  marked with  a (-)  in Mode. State  is unified  with the
%   result of the predicate abstract_interpreter/4, which is called inside.
%
%   Example:
%
%   consider the next predicate:
%
%   ```
%   popt('option'(A), [])    :- member(A, [1,2,3]).
%   popt('option 1', Answer) :- append(_,_,Answer).
%   popt('option 2', Answer) :- member(Answer, [1,2,3]).
%   popt('option 3', []) :- member(_Answer, [1,2,3]).
%   ```
%
%   If we just execute the predicate with Answer uninstatiated, we will get
%   infinite solutions, but:
%
%   ==
%   ?- abstract_slice(popt(A,X),[+,?],[]).
%   A = option(1) ;
%   A = option(2) ;
%   A = option(3) ;
%   A = 'option 1' ;
%   A = 'option 2' ;
%   A = 'option 3'.
%   ==
%
%   Will 'abstract' the execution of popt/2 to get all the matches of A, slicing
%   out X

abstract_slice(M:Head, Mode, OptL, State) :-
    apply_mode(Head, Mask, Mode, Spec, RevS),
    term_variables(RevS, VarsR),
    option(eval_scope(Scope), OptL, body),
    abstract_interpreter(M:Mask, slicer_abstraction(Spec, VarsR, Scope), OptL, State),
    % In Mask the output arguments are variable, so the binding is performed
    % after the abstract interpretation. This is a bit inefficient, but correct:
    Head = Mask.

apply_mode(Call, Mask, Mode, Spec, RevS) :-
    functor(Call, F, A),
    functor(Mask, F, A),
    functor(Spec, F, A),
    functor(RevS, F, A),
    apply_mode_arg(1, Call, Mask, Mode, Spec, RevS).

apply_mode_arg(N1, Call, Mask, Mode, Spec, RevS) :-
    arg(N1, Call, Arg), !,
    arg(N1, Mask, Var),
    arg(N1, Mode, MSp),
    arg(N1, Spec, ASp),
    arg(N1, RevS, ARs),
    ( MSp = -
    ->ASp = Var,
      ARs = -
    ; ASp = +,
      ARs = Arg,
      Arg = Var
    ),
    succ(N1, N),
    apply_mode_arg(N, Call, Mask, Mode, Spec, RevS).
apply_mode_arg(_, _, _, _, _, _).

chain_of_dependencies(Spec, VarsR, Goal, ContL) :-
    \+ ground(Goal),
    ( terms_share(Spec, VarsR, Goal)
    ->true
    ; select(Cont, ContL, ContL2),
      terms_share(Cont, VarsR, Goal),
      chain_of_dependencies(Spec, VarsR, Cont, ContL2)
    ),
    !.

slicer_abstraction(Spec, VarsR, Scope, MGoal, Body) -->
    {predicate_property(MGoal, interpreted)},
    !,
    {strip_module(MGoal, M, Goal)},
    get_state(state(Loc1, EvalL, OnErr, CallL, Data, Cont, Result1)),
    { \+ ground(Spec),
      chain_of_dependencies(Spec, VarsR, Goal, Cont)
    ->match_head_body(M:Goal, Body1, Loc),
      ( Scope = body
      ->( terms_share(Spec, VarsR, Goal)
        ->Body = Body1
        ; Body1 = CM:Body2,
          Body = CM:once(Body2)
        )
      ; terms_share(Spec, VarsR, Goal)
      ->Body = Body1
      ; Body = M:true
      )
    ; % check if the body trivially fails:
      ( Scope = body
      ->( % if no side effects, increase precision executing the body:
          ( match_head_body(M:Goal, Body1, Loc),
            is_pure_body(Body1, requires_subst(EvalL))
          ->call(Body1),
            Body = M:true
          ; match_head_body(M:Goal, Body1, Loc),
            ( is_pure_body(Body1)
            ->Body = Body1
            ; Body = M:true
            )
          )
        )
      ; Loc = Loc1,
        Body = M:true
      )
    },
    { Scope = head
    ->Result = bottom % Kludge to avoid cut remove solutions
    ; Result = Result1
    },
    put_state(state(Loc, EvalL, OnErr, CallL, Data, Cont, Result)).
slicer_abstraction(_, _, _, MGoal, M:true) -->
    get_state(state(Loc, _, OnError, CallL, _, _, _)),
    { call(OnError, error(existence_error(evaluation_rule, MGoal), Loc)),
      call(OnError, call_stack(CallL)),
      strip_module(MGoal, M, _)
    },
    bottom.

pattern_eval(H, I, _ +\ (I:H as _)).
pattern_eval(H, I, _ +\ (I:H :- _)).
pattern_eval(H, I, I:F/A) :- functor(H, F, A).

requires_subst(EvalL, M:H) :-
    predicate_property(M:H, implementation_module(I)),
    pattern_eval(H, I, Pattern),
    memberchk(Pattern, EvalL),
    !.

prolog:message(call_stack(CallL)) --> foldl(call_at, CallL).

call_at(Call-Loc) -->
    ["    "], '$messages':swi_location(Loc), ["~q"-[Call], nl].
