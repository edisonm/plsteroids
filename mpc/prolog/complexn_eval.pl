/*  CLP for multiple precision floating-point computation

    Author:        Edison Mera
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

:- module(complexn_eval, [int/3]).

:- use_module(library(solution_sequences)).
:- use_module(library(lists)).
:- use_module(library(neck)).
:- use_module(library(libcomplexn)).
:- use_module(library(libfloatn)).
:- use_module(library(compare_eq)).
:- use_module(library(list_sequence)).
:- use_module(library(complexn_desc)).
:- use_module(library(floatn_eval), []).
:- init_expansors.

:- include(library(floatn_reserve_eps)).

cd_prefix(complexn(R, I), complexn, [R, I]).

op_pred(Op, Pred) :- floatn_eval:op_pred(Op, Pred).

complexn_not_equal(A, B) :- \+ complexn_equal(A, B).

complexn_equal(A, B) :-
    inner_cast(_, A, C),
    inner_cast(_, B, D),
    complexn_cmp(R, C, D), R=:=0.

real_type(complexn(R, _), floatn(R)).

Body :-
    cd_prefix(Type, Pref, [R, I]),
    real_type(Type, RType),
    floatn_eval:cd_prefix(RType, RPre, _),
    op_pred(_, Pred),
    atomic_list_concat([Pref, '_', Pred], F),
    Body =.. [F, X, Y],
    \+ predicate_property(complexn_eval:Body, defined),
    atomic_list_concat([RPre, '_', Pred], G),
    Conv =.. [G, A, B],
    necki,
    complexn_real(R, I, A, X),
    complexn_real(R, I, B, Y),
    floatn_eval:Conv.

Head :-
    complexn_desc(pf_, FL, A),
    member(F, FL),
    atom_concat(complexn_w_, F, P),
    functor(Head, P, A),
    Head =.. [_, R, I, C|AL],
    atom_concat(complexn_, F, Q),
    Body =.. [Q, R, I, X|AL],
    necki,
    Body,
    complexn(X, R, I, C).

int(_, A, B) :-
    complexn_real(_, _, C, B),
    floatn_get_si(A, C).

expr_pred(A/B, div(A, B)).
expr_pred(A**B, pow(A, B)).
expr_pred(A^B, pow(A, B)).
expr_pred(A+B, add(A, B)).
expr_pred(A*B, mul(A, B)).
expr_pred(A-B, sub(A, B)).
expr_pred(-A, neg(A)).
% expr_pred(lgamma(A),   lngamma(A)).
% expr_pred(atan(A, B),  atan2(A, B)).
% expr_pred(integer(A),  rint_round(A)).
% expr_pred(round(A),    rint_round(A)).
% expr_pred(floor(A),    rint_floor(A)).
% expr_pred(ceiling(A),  rint_ceil(A)).
% expr_pred(ceil(A),     rint_ceil(A)).
% expr_pred(truncate(A), rint_trunc(A)).
% expr_pred(mod(A, B),   fmod(A, B)).
expr_pred(Expr, Pred) :-
    member(Pref-Desc, ['w_'-pf_, ''-pl_, ''-pc_]),
    complexn_desc(Desc, FL, A2),
    member(F, FL),
    A is A2 - 3,
    functor(Expr, F, A),
    atom_concat(Pref, F, P),
    Expr =.. [_|L],
    Pred =.. [P|L],
    \+ expr_pred(_, Pred),
    \+ expr_pred(Pred, _),
    neck.
expr_pred(Expr, Pred) :-
    floatn_eval:expr_pred(Expr, Pred),
    \+ expr_pred(Expr, Pred),
    neck.

eval_2([R, I], F, C) --> [complexn_real(R, I, C, F)].

Body :-
    cd_prefix(Type, Pref, EAL),
    real_type(Type, RType),
    floatn_eval:cd_prefix(RType, RPre, EFL),
    distinct(Pred, expr_pred(Expr, Pred)),
    once(floatn_eval:expr_pred(Expr, Pred)),
    Pred =.. [Name|AL],
    atomic_list_concat([Pref, '_', Name], BN),
    append(EAL, [C|AL], BL),
    Body =.. [BN|BL],
    \+ predicate_property(complexn_eval:Body, defined),
    atomic_list_concat([RPre, '_', Name], BC),
    foldl(eval_2(EAL), AL, FL, EvalL, [floatn_eval:Conv, inner_cast(Type, F, C)]),
    append(EFL, [F|FL], CL),
    Conv =.. [BC|CL],
    list_sequence(EvalL, EvalS),
    necki,
    EvalS.

is_complexn(X) :- complexn_t(X).

:- include(library(eval)).

do_eval(cputime, P, C) :- do_eval_cputime(P, C).
do_eval(epsilon, P, C) :- do_eval_epsilon(P, C).
do_eval(eval(A), P, C) :- eval(P, A, C).
do_eval(root(E, N), P, V) :- do_eval(E^(1/N), P, V).
do_eval(cbrt(E), P, V) :- do_eval(E^(1/3), P, V).
do_eval(+(A), P, C) :- eval(P, A, C).
do_eval(e, P, V) :- do_eval(exp(1), P, V).
do_eval(i, P, V) :- do_eval_i(P, V).
do_eval(sign(X), Type, C) :-
    eval(Type, X, V),
    do_eval_z(Type, Z),
    ( compare_b(=, Type, Z, V)
    ->C = Z
    ; do_eval(V/abs(V), Type, C)
    ).
do_eval((A, B), Type, C) :-
    do_eval_i(Type, I),
    do_eval(A+I*B, Type, C).
do_eval(Expr, Type, C) :-
    expr_pred(Expr, Pred),
    Pred =.. [Name|Args],
    maplist(eval_1(Type), Args, EvalL, EAs),
    AC =.. [Name, Type, C|EAs],
    list_sequence(EvalL, EvalS),
    necki,
    EvalS,
    AC.

do_eval_i(Type, C) :- cast(Type, "(0 1)", C).

do_eval_epsilon(complexn(NR, NI), V) :-
    N1 is 1 - min(NR, NI),
    complexn(2, NR, NI, F2),
    complexn(N1, NR, NI, FN),
    complexn_pow(NR, NI, V, F2, FN).
