/*  CLP over binary integer decimal numbers

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

:- module(bid_eval, [int/3]).

:- use_module(library(lists)).
:- use_module(library(libbid)).
:- use_module(library(compare_eq)).
:- use_module(library(neck)).
:- use_module(library(list_sequence)).
:- use_module(library(bid_desc)).
:- init_expansors.

cd_type(bid64).
cd_type(bid128).

cd_prefix(Type, Type, []) :-
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

desc(Desc, FL, A) :- bid_desc(Desc, FL, A).

is_bid64(X) :- bid64_t(X).

is_bid128(X) :- bid64_t(X).
is_bid128(X) :- bid128_t(X).

expr_pred(max(A, B), maxnum(A, B)).
expr_pred(min(A, B), minnum(A, B)).
expr_pred(floor(A), round_integral_negative(A)).
expr_pred(ceil(A), round_integral_positive(A)).
expr_pred(ceiling(A), round_integral_positive(A)).
expr_pred(integer(A), round_integral_exact(A)).
expr_pred(round(A), round_integral_exact(A)).
expr_pred(mod(A, B), fmod(A, B)).

:- include(library(dec_pred)).
:- include(library(eval)).
do_eval(abs(Expr), Type, C) :-
    eval(Type, Expr, V),
    do_eval_z(Type, Z),
    ( compare_b(>, Type, Z, V)
    ->do_eval(-V, Type, C)
    ; C = V
    ).
:- include(library(dec_const)).
