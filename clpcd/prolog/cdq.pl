/*  $Id$

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

:- module(cdq, []).

:- license(gpl_swipl, 'CLP(CDQ)').
:- use_module(library(neck)).
:- use_module(library(near_utils)).
:- use_module(library(cdqr), []).
:- reexport(library(clpcd)).
:- init_expansors.

clpcd_domain_ops:clpcd_module(cdq, cdq).

:- initialization(set_clpcd(cdq)).

clpcd_domain_ops:compare_d(cdq, Op, A, B) :-
    compare_q(Op, A, B).

compare_q(=,  A, B) :- A =:= B.
compare_q(=<, A, B) :- A =< B.
compare_q(>=, A, B) :- A >= B.
compare_q(<,  A, B) :- A < B.
compare_q(>,  A, B) :- A > B.
compare_q(\=, A, B) :- A =\= B.

clpcd_domain_ops:div_d(cdq, A, B, C) :- C is A rdiv B.

cdq_epsilon(R) :-
    repsilon(E),
    R is epsilon/E,
    neck.

clpcd_domain_ops:cast_d(cdq, A, B) :-
    cdq_epsilon(T),
    ( number(A)
    ->( A >= T
      ->B is rationalize(A)
      ; B is rational(A)
      )
    ; rational(A)
    ->B is rational(A)
    ).

clpcd_domain_ops:floor_d(cdq, A, B) :- B is floor(A).

clpcd_domain_ops:ceiling_d(cdq, A, B) :- B is ceiling(A).

clpcd_domain_ops:integerp(cdq, A, A) :- integer(A).
