/*

    Part of CLP(Q) (Constraint Logic Programming over Rationals)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2006, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is based on CLP(Q,R) by Christian Holzbaur for SICStus
    Prolog and distributed under the license details below with permission from
    all mentioned authors.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(cdq,
	  [ {}/1,
	    maximize/1,
	    minimize/1,
	    inf/2, inf/4, sup/2, sup/4,
	    bb_inf/3,
	    bb_inf/4,
	    ordering/1,
	    entailed/1
	  ]).

:- license(gpl_swipl, 'CLP(CDQ)').
:- use_module(library(clpcd/bb)).
:- use_module(library(clpcd/nf)).
:- use_module(library(clpcd/bv)).
:- reexport(library(clpcd/dump),
            [ dump/3 %, projecting_assert/1
            ]).
:- reexport(library(clpcd/itf), [clp_type/2]).
:- use_module(library(clpcd)).
:- reexport(library(clpcd/ordering), [ordering/1]).

clpcd_domain_ops:compare_d(cdq, Op, A, B) :-
    compare_q(Op, A, B).

compare_q(=,  A, B) :- A =:= B.
compare_q(=<, A, B) :- A =< B.
compare_q(>=, A, B) :- A >= B.
compare_q(<,  A, B) :- A < B.
compare_q(>,  A, B) :- A > B.
compare_q(\=, A, B) :- A =\= B.

clpcd_domain_ops:div_d(cdq, A, B, C) :- C is A rdiv B.

clpcd_domain_ops:cast_d(cdq, A, B) :-
    ( number(A)
    ; rational(A)
    ),
    !,
    B is rationalize(A).

clpcd_domain_ops:floor_d(cdq, A, B) :- B is floor(A).

clpcd_domain_ops:ceiling_d(cdq, A, B) :- B is ceiling(A).

clpcd_domain_ops:integerp(cdq, A, A) :- integer(A).

clpcd_itf:numbers_only(cdq, Y) :-
	(   var(Y)
	;   rational(Y)
	;   throw(type_error(_X = Y,2,'a rational number',Y))
	),
	!.

inf(Expression, Inf) :-
        inf(cdq, Expression, Inf).

inf(Expression, Inf, Vector, Vertex) :-
        inf(cdq, Expression, Inf, Vector, Vertex).

sup(Expression, Sup) :-
        sup(cdq, Expression, Sup).

sup(Expression, Sup, Vector, Vertex) :-
        sup(cdq, Expression, Sup, Vector, Vertex).

maximize(Term) :-
        maximize(cdq, Term).

minimize(Term) :-
        minimize(cdq, Term).

{Rel} :-
        add_constraint(Rel, cdq).

entailed(C) :- entailed(cdq, C).

bb_inf(Is, Term, Inf) :- bb_inf(cdq, Is, Term, Inf, _).
bb_inf(Is, Term, Inf, Vertex) :- bb_inf(cdq, Is, Term, Inf, Vertex).

		 /*******************************
		 *	       SANDBOX		*
		 *******************************/
:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(cdq:{_}).
sandbox:safe_primitive(cdq:entailed(_)).
