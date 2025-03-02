/*  CLP for multiple precision floating-point computation

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2025, Process Design Center, Breda, The Netherlands.
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

:- module(cdcomplexn, []).

:- license(gpl_swipl, 'CLP(COMPLEXN)').
:- use_module(library(complexn_eval)).
:- use_module(library(neck)).
:- use_module(library(clpcd/domain_ops)).
:- reexport(library(clpcd)).
:- init_expansors.

clpcd_domain_ops:clpcd_module(cdcomplexn(_), cdcomplexn).

cd_type(cdcomplexn(R, I), complexn(R, I)).

clpcd_domain_ops:rsgn_d(CDType, S, P, C) :-
    cd_type(CDType, Type),
    neck,
    between(Type, 0, P, N),
    eval(Type, exp((log(S)+(0, pi*(2*N)))/P), C).

between(_, Min, _, Min).
between(Type, Min, Max, Y) :-
    eval(Type, Min + 1, X),
    compare(Type, <, X, Max),
    between(Type, X, Max, Y).

cd_text(cdcomplexn(_, _), 'a multiple precison floating point number').

:- include(library(cd_common)).

:- initialization(set_clpcd(cdcomplexn(53,53))).
