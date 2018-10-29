/*  $Id$

    Part of CLP(R) (Constraint Logic Programming over Reals)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2004, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is part of Leslie De Koninck's master thesis, supervised
    by Bart Demoen and daily advisor Tom Schrijvers.  It is based on CLP(Q,R)
    by Christian Holzbaur for SICStus Prolog and distributed under the
    license details below with permission from all mentioned authors.

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



:- module(cdr,
	[
	    {}/1,
	    maximize/1,
	    minimize/1,
	    inf/2,
            inf/4,
            sup/2,
            sup/4,
	    bb_inf/3,
	    bb_inf/4,
	    entailed/1
	]).

:- license(gpl_swipl, 'CLP(CDR)').
:- use_module(library(clpcd/bb)).
:- use_module(library(clpcd/nf)).
:- use_module(library(clpcd/bv)).
:- use_module(library(clpcd/domain_ops)).
:- reexport(library(clpcd/dump),
            [ dump/3 %, projecting_assert/1
            ]).
:- reexport(library(clpcd/itf), [clp_type/2]).
:- use_module(library(clpcd)).
:- reexport(library(clpcd/ordering), [ordering/1]).
:- use_module(library(near_utils)).

clpcd_domain_ops:compare_d(cdr, Op, A, B) :-
    near_compare(Op, A, B).

clpcd_domain_ops:div_d(cdr, A, B, C) :- C is A/B.

clpcd_domain_ops:cast_d(cdr, A, B) :-
    ( number(A)
    ->B = A
    ; rational(A),
      A = X rdiv Y,
      B is X / Y
    ).

clpcd_domain_ops:floor_d(cdr, A, B) :-
    epsilon(abs(A), E),
    B is floor(A+E).

clpcd_domain_ops:ceiling_d(cdr, A, B) :-
    epsilon(abs(A), E),
    B is ceiling(A-E).

clpcd_domain_ops:integerp(cdr, X, I) :-
    I is round(X),
    compare_d(cdr, =, X, I).

clpcd_itf:numbers_only(cdr,Y) :-
	(   var(Y)
	;   integer(Y)
	;   float(Y)
        ;   rational(Y)
	;   throw(type_error(_X = Y,2,'a real number',Y))
	),
	!.

clpcd_highlight:clpcd_module(cdr).

inf(Expression, Inf) :-
        inf(cdr, Expression, Inf).

inf(Expression, Inf, Vector, Vertex) :-
        inf(cdr, Expression, Inf, Vector, Vertex).

sup(Expression, Sup) :-
        sup(cdr, Expression, Sup).

sup(Expression, Sup, Vector, Vertex) :-
        sup(cdr, Expression, Sup, Vector, Vertex).

maximize(Term) :-
        maximize(cdr, Term).

minimize(Term) :-
        minimize(cdr, Term).

{Rel} :-
        add_constraint(Rel, cdr).

entailed(C) :- entailed(cdr, C).

bb_inf(Is, Term, Inf) :- bb_inf(cdr, Is, Term, Inf, _).
bb_inf(Is, Term, Inf, Vertex) :- bb_inf(cdr, Is, Term, Inf, Vertex).

		 /*******************************
		 *	       SANDBOX		*
		 *******************************/
:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(cdr:{_}).
sandbox:safe_primitive(cdr:entailed(_)).
sandbox:safe_primitive(clpcd_bb:bb_inf(_, _, _)).
sandbox:safe_primitive(clpcd_bb:bb_inf(_, _, _, _)).
