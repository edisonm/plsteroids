/*  CLP for multiple precision floating-point computation

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

:- module(floatn_eval, []).

:- use_module(library(lists)).
:- use_module(library(neck)).
:- use_module(library(libfloatn)).
:- use_module(library(compare_eq)).
:- use_module(library(compilation_module)).
:- compilation_module(library(list_sequence)).
:- compilation_module(library(floatn_desc)).

:- pred [ [ e/2, epsilon/2, cputime/2 ] :: (int * -floatn_t),
          [ eval/3, (+)/3, (-)/3, lgamma/3
          ] :: (+floatn_t * int * -floatn_t),
          [ (/)/4, (+)/4, (*)/4, (-)/4, (**)/4, (^)/4, atan/4, root/4
          ] :: (+floatn_t * +floatn_t * int * -floatn_t)
        ].

cd_preffix(_, floatn).

op_pred(=,  equal).
op_pred(=<, lessequal).
op_pred(>=, greaterequal).
op_pred(<,  less).
op_pred(>,  greater).
op_pred(\=, not_equal).

reserve_eps(1024).

inner_cast(Type, Value, C) :-
    floatn(Value, Type, C).

floatn_not_equal(A, B) :- \+ floatn_equal(A, B).

:- include(library(eval)).

expr_pred(A/B, div(A, B)).
expr_pred(A**B, pow(A, B)).
expr_pred(A^B, pow(A, B)).
expr_pred(A+B, add(A, B)).
expr_pred(A*B, mul(A, B)).
expr_pred(A-B, sub(A, B)).
expr_pred(-A, neg(A)).
expr_pred(lgamma(A),   lngamma(A)).
expr_pred(atan(A, B),  atan2(A, B)).
expr_pred(integer(A),  rint_round(A)).
expr_pred(floor(A),    rint_floor(A)).
expr_pred(ceiling(A),  rint_ceil(A)).
expr_pred(ceil(A),     rint_ceil(A)).
expr_pred(truncate(A), rint_trunc(A)).
expr_pred(Pred, Pred) :-
    member(Desc, [pl_, pc_]),
    floatn_desc(Desc, FL, A2),
    member(F, FL),
    A is A2 - 2,
    functor(Pred, F, A),
    \+ expr_pred(_, Pred),
    \+ expr_pred(Pred, _),
    neck.

do_eval(cputime, P, C) :- do_eval_cputime(P, C).
do_eval(epsilon, P, C) :- do_eval_epsilon(P, C).
do_eval(eval(A), P, C) :- eval(P, A, C).
do_eval(root(E, N), P, V) :- do_eval(E^(1/N), P, V).
do_eval(+(A), P, C) :- eval(P, A, C).
do_eval(e, P, V) :-
    floatn(1, P, F1),
    floatn_exp(P, V, F1).
do_eval(sign(X), Type, C) :-
    eval(Type, X, V),
    do_eval_z(Type, Z),
    ( compare_b(<, Type, Z, V)
    ->do_eval_1(Type, C)
    ; compare(Type, >, Z, X)
    ->do_eval_m1(Type, C)
    ; C = Z
    ).
do_eval(Expr, P, C) :-
    expr_pred(Expr, Pred),
    Pred =.. [Name|Args],
    maplist(eval_1(P), Args, EvalL, EAs),
    atomic_list_concat([floatn, '_', Name], BN),
    AC =.. [BN, P, C|EAs],
    list_sequence(EvalL, EvalS),
    necki,
    EvalS,
    AC.

do_eval_z(Type, C) :- cast(Type, 0, C).

do_eval_1(Type, C) :- cast(Type, 1, C).

do_eval_m1(Type, C) :- cast(Type, -1, C).

do_eval_cputime(P, V) :-
    X is cputime,
    floatn(X, P, V).

do_eval_epsilon(P, V) :-
    ( var(P)
    ->mpfr_get_default_prec(N)
    ; N=P
    ),
    N1 is 1-N,
    floatn(2,  P, F2),
    floatn(N1, P, FN),
    floatn_pow(P, V, F2, FN).
