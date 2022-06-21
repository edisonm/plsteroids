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

:- module(clpcd,
          [ {}/1,
            maximize/1,
            minimize/1,
            inf/2,
            inf/4,
            sup/2,
            sup/4,
            bb_inf/3,
            bb_inf/4,
            entailed/1,
            set_clpcd/1
          ]).

:- use_module(library(lists)).
:- use_module(library(neck)).
:- use_module(library(clpcd/bb)).
:- use_module(library(clpcd/nf)).
:- use_module(library(clpcd/bv)).
:- use_module(library(clpcd/inv)).
:- use_module(library(clpcd/domain_ops)).
:- reexport(library(clpcd/dump),       [dump/3]).
:- reexport(library(clpcd/ordering),   [clp_type/2]).
:- reexport(library(clpcd/ordering),   [ordering/1]).
:- reexport(library(clpcd/domain_ops), [set_clpcd/1]).

inf(Expression, Inf) :-
    active_clpcd(D),
    inf(D, Expression, Inf).

inf(Expression, Inf, Vector, Vertex) :-
    active_clpcd(D),
    inf(D, Expression, Inf, Vector, Vertex).

sup(Expression, Sup) :-
    active_clpcd(D),
    sup(D, Expression, Sup).

sup(Expression, Sup, Vector, Vertex) :-
    active_clpcd(D),
    sup(D, Expression, Sup, Vector, Vertex).

maximize(Term) :-
    active_clpcd(D),
    maximize(D, Term).

minimize(Term) :-
    active_clpcd(D),
    minimize(D, Term).

{Rel} :-
    active_clpcd(D),
    add_constraint(Rel, D).

entailed(C) :-
    active_clpcd(D),
    entailed(D, C).

bb_inf(Is, Term, Inf) :-
    active_clpcd(D),
    bb_inf(D, Is, Term, Inf, _).

bb_inf(Is, Term, Inf, Vertex) :-
    active_clpcd(D),
    bb_inf(D, Is, Term, Inf, Vertex).

clpcd_nf:nl_invertible(_, F) :-
    cd_invertible(F).

clpcd_nf:nl_invert(C, F, X, Y, Res) :-
    cd_invert(F, C, X, Y, N),
    cast_d(C, N, Res).

clpcd_nf:nonlin(_, Term, AL, Skel, SL) :-
    cd_nonlin(Term, AL, Skel, SL).

:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(clpcd:H) :-
    member(H, [{_},
               entailed(_),
               bb_inf(_, _, _),
               bb_inf(_, _, _, _),
               maximize(_),
               minimize(_),
               inf(_,_),
               inf(_,_,_,_),
               sup(_,_),
               sup(_,_,_,_)
              ]),
    neck.
