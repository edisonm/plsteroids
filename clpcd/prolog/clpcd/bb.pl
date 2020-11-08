/*  $Id$

    Part of CPL(R) (Constraint Logic Programming over Reals)

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

:- module(clpcd_bb,
	[
	    bb_inf/5
	]).

:- use_module(library(clpcd/domain_ops)).
:- use_module(library(clpcd/bv)).
:- use_module(library(clpcd/nf)).
:- use_module(library(clpcd/solve)).
:- use_module(library(clpcd/detact)).

% bb_inf(Ints,Term,Inf)
%
% Finds the infimum of Term where the variables Ints are to be integers.
% The infimum is stored in Inf.

bb_inf(CLP, Is, Term, Inf, Vertex) :-
	wait_linear(CLP, Term, Nf, bb_inf_internal(CLP, Is, Nf, Inf, Vertex)).

% ---------------------------------------------------------------------

% bb_inf_internal(Is,Lin,Inf,Vertex)
%
% Finds an infimum Inf for linear expression in normal form Lin, where
% all variables in Is are to be integers.

bb_inf_internal(CLP, Is, Lin, _, _) :-
	bb_intern(Is, CLP, IsNf),
	nb_delete(prov_opt),
	repair(CLP, Lin, LinR),	% bb_narrow ...
	deref(CLP,LinR,Lind),
	var_with_def_assign(CLP, Dep, Lind),
	determine_active_dec(CLP, Lind),
	bb_loop(CLP, Dep, IsNf),
	fail.
bb_inf_internal(CLP, _, _, Inf, Vertex) :-
	nb_current(prov_opt,InfVal-Vertex),
	add_constraint(Inf =:= InfVal, CLP),
	nb_delete(prov_opt).

% bb_loop(CLP, Opt, Is)
%
% Minimizes the value of Opt where variables Is have to be integer values.
% This predicate can be backtracked to try different strategies.

bb_loop(CLP, Opt, Is) :-
	bb_reoptimize(CLP, Opt, Inf),
	bb_better_bound(CLP, Inf),
	vertex_value(Is, CLP, Ivs),
	(   bb_first_nonint(CLP, Is, Ivs, Viol, Floor, Ceiling)
	->  bb_branch(CLP, Viol, Floor, Ceiling),
	    bb_loop(CLP, Opt, Is)
	;   round_values(Ivs,CLP,RoundVertex),
	    nb_setval(prov_opt,Inf-RoundVertex) % new provisional optimum
	).

% bb_reoptimize(Obj,Inf)
%
% Minimizes the value of Obj and puts the result in Inf.
% This new minimization is necessary as making a bound integer may yield a
% different optimum. The added inequalities may also have led to binding.

bb_reoptimize(CLP, Obj, Inf) :-
	var(Obj),
        !,
	iterate_dec(CLP, Obj, Inf).
bb_reoptimize(_, Obj, Inf) :-
	Inf = Obj.

% bb_better_bound(Inf)
%
% Checks if the new infimum Inf is better than the previous one (if such exists).

bb_better_bound(CLP, Inf) :-
	nb_current(prov_opt,Inc-_), !,
	compare_d(CLP, <, Inf, Inc).
bb_better_bound(_, _).

% bb_branch(V,U,L)
%
% Stores that V =< U or V >= L, can be used for different strategies within bb_loop/3.

bb_branch(CLP, V, U, _) :- add_constraint(V =< U, CLP).
bb_branch(CLP, V, _, L) :- add_constraint(V >= L, CLP).

% bb_first_nonint(Ints,Rhss,Viol,Floor,Ceiling)
%
% Finds the first variable in Ints which doesn't have an active integer bound.
% Rhss contain the Rhs (R + I) values corresponding to the variables.
% The first variable that hasn't got an active integer bound, is returned in
% Viol. The floor and ceiling of its actual bound is returned in Floor and Ceiling.

bb_first_nonint(CLP, [I|Is], [Rhs|Rhss], Viol, F, C) :-
	(   floor_d(CLP, Rhs, Floor),
            ceiling_d(CLP, Rhs, Ceiling),
            compare_d(CLP, <, epsilon, min(Rhs-Floor, Ceiling-Rhs))
	->  Viol = I,
            F = Floor,
            C = Ceiling
        ;   bb_first_nonint(CLP, Is, Rhss, Viol, F, C)
	).

% round_values([X|Xs],CLP,[Xr|Xrs])
%
% Rounds of the values of the first list into the second list.

round_values([],_,[]).
round_values([X|Xs],CLP,[Y|Ys]) :-
        eval_d(CLP, round(X), Y),
	round_values(Xs,CLP,Ys).

% bb_intern([X|Xs],[Xi|Xis])
%
% Turns the elements of the first list into integers into the second
% list via bb_intern/3.

bb_intern([], _, []).
bb_intern([X|Xs], CLP, [Xi|Xis]) :-
	nf(X, CLP, Xnf),
	bb_intern(Xnf, CLP, Xi, X),
	bb_intern(Xs, CLP, Xis).


% bb_intern(Nf,X,Term)
%
% Makes sure that Term which is normalized into Nf, is integer.
% X contains the possibly changed Term. If Term is a variable,
% then its bounds are hightened or lowered to the next integer.
% Otherwise, it is checked it Term is integer.

bb_intern([], _, X, _) :-
	!,
	X = 0.
bb_intern([v(I,[])], CLP, X, _) :-
	!,
	X = I,
        compare_d(CLP, =, I, integer(I)).
bb_intern([v(One,[V^1])], CLP, X, _) :-
        compare_d(CLP, =, One, 1),
	!,
	V = X,
	bb_narrow_lower(CLP, X),
	bb_narrow_upper(CLP, X).
bb_intern(_, CLP, _, Term) :-
	throw(instantiation_error(bb_inf(CLP, Term, _, _),1)).

% bb_narrow_lower(X)
%
% Narrows the lower bound so that it is an integer bound.
% We do this by finding the infimum of X and asserting that X
% is larger than the first integer larger or equal to the infimum
% (second integer if X is to be strict larger than the first integer).

bb_narrow_lower(CLP, X) :-
	(   inf(CLP, X, Inf)
	->  ceiling_d(CLP, Inf, Bound),
	    (   entailed(CLP, X > Bound)
            ->  add_constraint(X >= Bound+1, CLP)
	    ;   add_constraint(X >= Bound,   CLP)
	    )
	;   true
	).

% bb_narrow_upper(X)
%
% See bb_narrow_lower/1. This predicate handles the upper bound.

bb_narrow_upper(CLP, X) :-
	(   sup(CLP, X, Sup)
	->  floor_d(CLP, Sup, Bound),
	    (   entailed(CLP, X < Bound)
            ->  add_constraint(X =< Bound-1, CLP)
	    ;   add_constraint(X =< Bound,   CLP)
	    )
	;   true
	).
