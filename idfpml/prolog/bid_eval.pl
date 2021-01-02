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

:- module(bid_eval, [int/3]).

:- use_module(library(lists)).
:- use_module(library(neck)).
:- use_module(library(libbid)).
:- use_module(library(compare_eq)).
:- use_module(library(compilation_module)).
:- compilation_module(library(list_sequence)).
:- compilation_module(library(bid_desc)).
:- compilation_predicate cd_type/1.

cd_type(bid64).
cd_type(bid128).

cd_preffix(Type, Type, []) :-
    cd_type(Type),
    neck.

int(bid64,  A, B) :- bid64_int( A, B).
int(bid128, A, B) :- bid128_int(A, B).

op_pred(=,  quiet_equal).
op_pred(=<, quiet_less_equal).
op_pred(>=, quiet_greater_equal).
op_pred(<,  quiet_less).
op_pred(>,  quiet_greater).
op_pred(\=, quiet_not_equal).

reserve_eps(1000 ).

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

is_bid64(X) :- bid64_t(X).

is_bid128(X) :- bid64_t(X).
is_bid128(X) :- bid128_t(X).

:- include(library(eval)).

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

do_eval_z(Type, C) :-
    cd_type(Type),
    cast(Type, 0, C),
    neck.

do_eval_1(Type, C) :-
    cd_type(Type),
    cast(Type, 1, C),
    neck.

do_eval_m1(Type, C) :-
    cd_type(Type),
    cast(Type, -1, C),
    neck.

do_eval_e(Type, C) :-
    cd_type(Type),
    do_eval_1(Type, F1),
    exp(Type, C, F1),
    neck.

do_eval_pi(Type, C) :-
    cd_type(Type),
    do_eval(4*atan(1), Type, C),
    neck.

do_eval_epsilon(Type, E) :-
    cd_type(Type),
    eval(Type, '0.1', P),
    do_eval_1(Type, O),
    once(( between(1,200,X),
           do_eval(P^X, Type, E),
           do_eval(O+E, Type, Y),
           compare_b(=, Type, Y, O)
         )),
    neck.
