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

:- module(clpcd_bv,
          [
              allvars/2,
              detach_bounds/2,
              detach_bounds_vlv/6,
              iterate_dec/3,
              var_with_def_assign/3,
              vertex_value/3,
              maximize/2,
              minimize/2,
              sup/3,
              sup/5,
              inf/3,
              inf/5
          ]).

:- use_module(library(clpcd/class)).
:- use_module(library(clpcd/domain_ops)).
:- use_module(library(clpcd/indep)).
:- use_module(library(clpcd/nf)).
:- use_module(library(clpcd/solve)).
:- use_module(library(clpcd/store)).
:- use_module(library(clpcd/detact)).

% For the rhs maint. the following events are important:
%
%	-) introduction of an indep var at active bound B
%	-) narrowing of active bound
%	-) swap active bound
%	-) pivot
%

% a variables bound (L/U) can have the states:
%
%	-) t_none	no bounds
%	-) t_l		inactive lower bound
%	-) t_u		inactive upper bound
%	-) t_L		active lower bound
%	-) t_U		active upper bound
%	-) t_lu		inactive lower and upper bound
%	-) t_Lu		active lower bound and inactive upper bound
%	-) t_lU		inactive lower bound and active upper bound

% TODO
%
%

var_with_def_assign(CLP,Var,Lin) :-
	Lin = [I,_|Hom],
	(   Hom = []
	->  % X=k
	    Var = I
	;   Hom = [l(V*K,_)|Cs]
	->  (   Cs = [],
		compare_d(CLP, =, K, 1),	% K =:= 1
		compare_d(CLP, =, 0, I)
	    ->	% X=Y
		Var = V
	    ;	% general case
		var_with_def_intern(t_none,CLP,Var,Lin,0)
	    )
	).

maximize(CLP, Term) :-
	minimize(CLP, -Term).

%
% This is NOT coded as minimize(Expr) :- inf(Expr,Expr).
%
% because the new version of inf/2 only visits
% the vertex where the infimum is assumed and returns
% to the 'current' vertex via backtracking.
% The rationale behind this construction is to eliminate
% all garbage in the solver data structures produced by
% the pivots on the way to the extremal point caused by
% {inf,sup}/{2,4}.
%
% If we are after the infimum/supremum for minimizing/maximizing,
% this strategy may have adverse effects on performance because
% the simplex algorithm is forced to re-discover the
% extremal vertex through the equation {Inf =:= Expr}.
%
% Thus the extra code for {minimize,maximize}/1.
%
% In case someone comes up with an example where
%
%   inf(Expr,Expr)
%
% outperforms the provided formulation for minimize - so be it.
% Both forms are available to the user.
%
minimize(CLP,Term) :-
	wait_linear(CLP, Term, Nf, minimize_lin(CLP,Nf)).

% minimize_lin(CLP,Lin)
%
% Minimizes the linear expression Lin. It does so by making a new
% variable Dep and minimizes its value.

minimize_lin(CLP,Lin) :-
	deref(CLP,Lin,Lind),
	var_with_def_intern(t_none,CLP,Dep,Lind,0),
	determine_active_dec(CLP, Lind),
	iterate_dec(CLP, Dep, Inf),
	add_constraint(Dep =:= Inf, CLP).

sup(CLP,Expression,Sup) :-
	sup(CLP,Expression,Sup,[],[]).

sup(CLP,Expression,Sup,Vector,Vertex) :-
	inf(CLP,-Expression,-Sup,Vector,Vertex).

inf(CLP,Expression,Inf) :-
	inf(CLP,Expression,Inf,[],[]).

inf(CLP,Expression,Inf,Vector,Vertex) :-
	% wait until Expression becomes linear, Nf contains linear Expression
	% in normal form
	wait_linear(CLP, Expression, Nf, inf_lin(CLP,Nf,Inf,Vector,Vertex)).

inf_lin(CLP, Lin,_,Vector,_) :-
	deref(CLP,Lin,Lind),
	var_with_def_intern(t_none,CLP,Dep,Lind,0),	% make new variable Dep = Lind
	determine_active_dec(CLP, Lind),	% minimizes Lind
	iterate_dec(CLP, Dep, Inf),
	vertex_value(Vector, CLP, Values),
	nb_setval(inf,[Inf|Values]),
	fail.
inf_lin(CLP, _,Infimum,_,Vertex) :-
	nb_current(inf,L),
	nb_delete(inf),
	assign([Infimum|Vertex],CLP,L).

% vertex_value(Vars,Values)
%
% Returns in <Values> the current values of the variables in <Vars>.

vertex_value([], _, []).
vertex_value([X|Xs], CLP, [V|Vs]) :-
	rhs_value(CLP, X, V),
	vertex_value(Xs, CLP, Vs).

% rhs_value(X,Value)
%
% Returns in <Value> the current value of variable <X>.

rhs_value(CLP, Xn, Value) :-
	(   nonvar(Xn)
	->  Value = Xn
	;   var(Xn)
	->  deref_var(CLP, Xn, Xd),
	    Xd = [I,R|_],
	    eval_d(CLP, R+I, Value)
	).

% assign(L1,CLP,L2)
%
% The elements of L1 are pairwise assigned to the elements of L2
% by means of asserting {X =:= Y} where X is an element of L1 and Y
% is the corresponding element of L2.

assign([],_,[]).
assign([X|Xs],CLP,[Y|Ys]) :-
	add_constraint(X =:= Y, CLP), % more defensive/expressive than X=Y
	assign(Xs,CLP,Ys).

% --------------------------------- optimization ------------------------------
%
% The _sn(S) =< 0 row might be temporarily infeasible.
% We use reconsider/2 to fix this.
%
%   s(S) e [_,0] = d +xi ... -xj, Rhs > 0 so we want to decrease s(S)
%
%   positive xi would have to be moved towards their lower bound,
%   negative xj would have to be moved towards their upper bound,
%
%   the row s(S) does not limit the lower bound of xi
%   the row s(S) does not limit the upper bound of xj
%
%   a) if some other row R is limiting xk, we pivot(R,xk),
%      s(S) will decrease and get more feasible until (b)
%   b) if there is no limiting row for some xi: we pivot(s(S),xi)
%					    xj: we pivot(s(S),xj)
%      which cures the infeasibility in one step
%


% iterate_dec(OptVar,Opt)
%
% Decreases the bound on the variables of the linear equation of OptVar as much
% as possible and returns the resulting optimal bound in Opt. Fails if for some
% variable, a status of unlimited is found.

iterate_dec(CLP, OptVar, Opt) :-
	get_attr(OptVar,clpcd_itf,Att),
	arg(4,Att,lin([I,R|H])),
	dec_step(H, CLP, Status),
	(   Status = applied
	->  iterate_dec(CLP, OptVar, Opt)
	;   Status = optimum,
	    eval_d(CLP, R + I, Opt)
	).

% allvars(X,Allvars)
%
% Allvars is a list of all variables in the class to which X belongs.

allvars(X,Allvars) :-
	get_attr(X,clpcd_itf,Att),
	arg(6,Att,class(C)),
	class_allvars(C,Allvars).

%
% Careful when an indep turns into t_none !!!
%

detach_bounds(CLP, V) :-
	get_attr(V,clpcd_itf,Att),
	arg(2,Att,type(Type)),
	arg(4,Att,lin(Lin)),
	arg(5,Att,order(OrdV)),
	arg(6,Att,class(Class)),
	setarg(2,Att,type(t_none)),
	setarg(3,Att,strictness(0)),
	(   indep(CLP, Lin, OrdV)
	->  (   ub(Class,CLP,OrdV,Vub-Vb-_)
	    ->	% exchange against thightest
		class_basis_drop(Class,Vub),
		pivot(CLP, Vub, Class, OrdV, Vb, Type)
	    ;   lb(Class,CLP,OrdV,Vlb-Vb-_)
	    ->  class_basis_drop(Class,Vlb),
		pivot(CLP, Vlb, Class, OrdV, Vb, Type)
	    ;   true
	    )
	;   class_basis_drop(Class,V)
	).

detach_bounds_vlv(CLP,OrdV,Lin,Class,Var,NewLin) :-
	(   indep(CLP, Lin, OrdV)
	->  Lin = [_,R|_],
	    (   ub(Class,CLP,OrdV,Vub-Vb-_)
	    ->  % in verify_lin, class might contain two occurrences of Var,
		% but it doesn't matter which one we delete
		class_basis_drop(Class,Var),
		pivot_vlv(CLP, Vub, Class, OrdV, Vb, R, NewLin)
	    ;   lb(Class,CLP,OrdV,Vlb-Vb-_)
	    ->  class_basis_drop(Class,Var),
		pivot_vlv(CLP, Vlb, Class, OrdV, Vb, R, NewLin)
	    ;   NewLin = Lin
	    )
	;   NewLin = Lin,
	    class_basis_drop(Class,Var)
	).

% Rewrite Dep = ... + Coeff*Indep + ...
% into Indep = ... + -1/Coeff*Dep + ...
%
% For backsubstitution, old current value of Indep must be removed from RHS
% New current value of Dep must be added to RHS
% For solving: old current value of Indep should be out of RHS

pivot_vlv(CLP,Dep,Class,IndepOrd,DepAct,AbvI,Lin) :-
	get_attr(Dep,clpcd_itf,Att),
	arg(4,Att,lin(H)),
	arg(5,Att,order(DepOrd)),
	setarg(2,Att,type(DepAct)),
	select_active_bound(DepAct,AbvD), % New current value for Dep
	delete_factor(IndepOrd,H,H0,Coeff), % Dep = ... + Coeff*Indep + ...
	eval_d(CLP, -AbvD, AbvDm),
	eval_d(CLP, -AbvI, AbvIm),
	add_linear_f1(CLP, [0,AbvIm], Coeff, H0, H1),
	div_d(CLP, -1, Coeff, K),
	add_linear_ff(CLP, H1, K, [0,AbvDm,l(Dep* -1,DepOrd)], K, Lin),
	    % Indep = -1/Coeff*... + 1/Coeff*Dep
	add_linear_11(CLP, Lin, [0,AbvIm], SubstLin),
	backsubst(CLP, Class, IndepOrd, SubstLin).
