/*  Constraint logic programming over continuous domains

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

:- use_module(library(clpcd/domain_ops)).

:- public cd_type/2.
:- public cd_text/2.

clpcd_domain_ops:compare_d(Domain, Op, A, B) :-
    cd_type(Domain, Type),
    neck,
    near_compare(Type, Op, A, B).

clpcd_domain_ops:eval_d(C, F, R) :-
    cd_type(C, T),
    neck,
    eval(T, F, R).

clpcd_domain_ops:div_d(Domain, A, B, C) :-
    cd_type(Domain, Type),
    neck,
    eval(Type, A/B, C).

clpcd_domain_ops:cast_d(Domain, A, B) :-
    cd_type(Domain, Type),
    neck,
    cast(Type, A, B).

clpcd_domain_ops:floor_d(Domain, A, B) :-
    cd_type(Domain, Type),
    neck,
    eepsilon(Type, abs(A), E),
    eval(Type, floor(A+E), B).

clpcd_domain_ops:ceiling_d(Domain, A, B) :-
    cd_type(Domain, Type),
    neck,
    eepsilon(Type, abs(A), E),
    eval(Type, ceiling(A-E), B).

clpcd_domain_ops:integerp(Domain, A, C) :-
    cd_type(Domain, Type),
    neck,
    eval(Type, integer(A), B),
    compare(=, A, B), % near_compare(=, A, B)
    int(Type, C, B).

clpcd_domain_ops:numbers_only(Domain, Y) :-
    cd_type(Domain, Type),
    cd_text(Domain, Text),
    neck,
    (   var(Y)
    ;   number(Y)
    ;   castable(Type, Y)
    ;   throw(type_error(_X = Y,2,Text,Y))
    ),
    !.
