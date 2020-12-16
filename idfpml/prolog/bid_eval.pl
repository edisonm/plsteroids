/*  CLP over binary integer decimal numbers

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
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

:- module(bid_eval,
          [ bid_t/1,
            cast/3,
            compare/4,
            epsilon/2,
            epsilon/3,
            eval/3,
            int/3,
            near_compare/4
          ]).

:- use_module(library(lists)).
:- use_module(library(neck)).
:- use_module(library(libbid)).
:- use_module(library(compare_eq)).
:- use_module(library(compilation_module)).
:- compilation_module(library(list_sequence)).
:- compilation_module(library(bid_desc)).

bid_t(bid64).
bid_t(bid128).

:- public
        op_pred/2,
        pred_expr/2,
        eval_1/4.

op_pred(=,  quiet_equal).
op_pred(=<, quiet_less_equal).
op_pred(>=, quiet_greater_equal).
op_pred(<,  quiet_less).
op_pred(>,  quiet_greater).
op_pred(\=, quiet_not_equal).

compare(Type, Op, A, B) :-
    eval(Type, A, X),
    eval(Type, B, Y),
    compare_b(Op, Type, X, Y).

near_compare(Type, Op, A, B) :-
    eval(Type, A, X),
    eval(Type, B, Y),
    near_compare_b(Type, Op, X, Y).

near_compare_b(Type, Op, X, Y) :-
    ( compare_b(=, Type, X, Y)
    ->compare_eq(Op)
    ; epsilon(Type, max(abs(X), abs(Y)), E),
      compare(Op, Type, X, Y, E)
    ).

epsilon(T, E) :-
    eval(T, 1000*epsilon, E).

epsilon(T, N, E) :-
    epsilon(T, R),
    eval(T, R*N, E).

compare(=,  T, A, B, E) :- compare(T, =<, abs(A - B), E).
compare(=<, T, A, B, E) :- compare(T, =<, A - B, E).
compare(>=, T, A, B, E) :- compare(T, =<, B - A, E).
compare(<,  T, A, B, E) :- compare(T, >, B - A, E).
compare(>,  T, A, B, E) :- compare(T, >, A - B, E).
compare(\=, T, A, B, E) :- compare(T, >, abs(A - B), E).

compare_b(Op, Type, X, Y) :-
    op_pred(Op, Pred),
    Body =.. [Pred, Type, X, Y],
    necki,
    Body.

Head :-
    op_pred(_, Pred),
    Head =.. [Pred, Type, X, Y],
    bid_t(Type),
    atomic_list_concat([Type, '_', Pred], F),
    Body =.. [F, X, Y],
    necki,
    Body.

eval_1(Type, Arg, eval(Type, Arg, EA), EA).

eval(_, Expr, _) :-
    var(Expr),
    !,
    fail.
eval(Type, Expr, C) :-
    do_eval(Expr, Type, C),
    !.
eval(Type, Value, C) :-
    cast(Type, Value, C).

inner_cast(Type, Value, C) :-
    bid_t(Type),
    Body =.. [Type, Value, C],
    neck,
    Body.

cast(Type, Value, C) :-
    ( inner_cast(Type, Value, C)
    ->true
    ; integer(Value)
    ->term_string(Value, String),
      cast(Type, String, C)
    ; rational(Value)
    ->X is numerator(Value),
      Y is denominator(Value),
      do_eval(X/Y, Type, C)
    ).

:- compilation_predicate expr_pred/2.

expr_pred(atan(A, B), atan2(A, B)).
% expr_pred(copysign(A, B), copysign(A, B)).
expr_pred(A/B, div(A,B)).
expr_pred(A**B, pow(A,B)).
% expr_pred(float(A), float(A)).
% expr_pred(float_fractional_part(A), float_fractional_part(A)).
% expr_pred(float_integer_part(A), float_integer_part(A)).
expr_pred(A^B, pow(A, B)).
% expr_pred(inf, inf).
expr_pred(max(A, B), maxnum(A, B)).
expr_pred(min(A, B), minnum(A, B)).
expr_pred(A-B, sub(A, B)).
% expr_pred(nan, nan).
% expr_pred(nexttoward(A, B), nexttoward(A, B)).
expr_pred(A+B, add(A, B)).
% expr_pred(random_float, random_float).
expr_pred(A*B, mul(A, B)).
expr_pred(floor(A), round_integral_negative(A)).
expr_pred(ceil(A), round_integral_positive(A)).
expr_pred(ceiling(A), round_integral_positive(A)).
expr_pred(integer(A), round_integral_exact(A)).
expr_pred(round(A), round_integral_exact(A)).
expr_pred(mod(A, B), fmod(A, B)).
expr_pred(Pred, Pred) :-
    member(Desc, [pl_, pn_]),
    bid_desc(Desc, FL, A1),
    member(F, FL),
    succ(A, A1),
    functor(Pred, F, A),
    \+ expr_pred(_, Pred),
    \+ expr_pred(Pred, _),
    neck.

do_eval(cputime, Type, C) :-
    X is cputime,
    cast(Type, X, C).
do_eval(epsilon, Type, C) :- do_eval_epsilon(Type, C).
do_eval(0,  Type, C) :- do_eval_z(Type, C).
do_eval(1,  Type, C) :- do_eval_1(Type, C).
do_eval(-1, Type, C) :- do_eval_m1(Type, C).
do_eval(e,  Type, C) :- do_eval_e(Type, C).
do_eval(pi, Type, C) :- do_eval_pi(Type, C).
do_eval(sign(X), Type, C) :-
    eval(Type, X, V),
    do_eval_z(Type, Z),
    ( compare_b(<, Type, Z, V)
    ->do_eval_1(Type, C)
    ; compare(Type, >, Z, X)
    ->do_eval_m1(Type, C)
    ; C = Z
    ).
do_eval(eval(Expr), Type, C) :- eval(Type, Expr, C).
do_eval(+(Expr), Type, C) :- eval(Type, Expr, C).
do_eval(-(Expr), Type, C) :- do_eval(0-Expr, Type, C).
do_eval(abs(Expr), Type, C) :-
    eval(Type, Expr, V),
    do_eval_z(Type, Z),
    ( compare_b(>, Type, Z, V)
    ->do_eval(-V, Type, C)
    ; C = V
    ).
do_eval(Expr, Type, C) :-
    expr_pred(Expr, Pred),
    Pred =.. [Name|Args],
    maplist(eval_1(Type), Args, EvalL, EAs),
    AC =.. [Name, Type, C|EAs],
    list_sequence(EvalL, EvalS),
    necki,
    EvalS,
    AC.

Head :-
    bid_desc(Desc, FL, A),
    ( memberchk(Desc, [pl_, pn_]),
      member(F, FL)
    ; Desc = pi_,
      member(F/A, [int/2]),
      memberchk(F, FL)
    ),
    functor(Pred, F, A),
    Pred =.. [N, Result|AL],
    Head =.. [N, Type, Result|AL],
    bid_t(Type),
    atomic_list_concat([Type, '_', N], BN),
    Body =.. [BN, Result|AL],
    necki,
    Body.

do_eval_z(Type, C) :-
    bid_t(Type),
    cast(Type, 0, C),
    neck.

do_eval_1(Type, C) :-
    bid_t(Type),
    cast(Type, 1, C),
    neck.

do_eval_m1(Type, C) :-
    bid_t(Type),
    cast(Type, -1, C),
    neck.

do_eval_e(Type, C) :-
    bid_t(Type),
    do_eval(exp(1), Type, C),
    neck.

do_eval_pi(Type, C) :-
    bid_t(Type),
    do_eval(4*atan(1), Type, C),
    neck.

do_eval_epsilon(Type, E) :-
    bid_t(Type),
    eval(Type, '0.1', P),
    do_eval_1(Type, O),
    once(( between(1,200,X),
           do_eval(P^X, Type, E),
           do_eval(O+E, Type, Y),
           compare_b(=, Type, Y, O)
         )),
    neck.
