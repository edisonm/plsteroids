/*

    Part of CLP(Q,R) (Constraint Logic Programming over Rationals and Reals)

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

%
% Answer constraint projection
%

:- module(clpcd_project,
	[
	    drop_dep/1,
	    drop_dep_one/1,
	    make_target_indep/2
	]).

:- use_module(library(clpcd/indep)).
:- use_module(library(clpcd/solve)).

%
% Collect the pivots in reverse order
% We have to protect the target variables pivot partners
% from redundancy eliminations triggered by fm_elim,
% in order to allow for reverse pivoting.
%
make_target_indep(Ts,Ps) :- make_target_indep(Ts,[],Ps).

% make_target_indep(Targets,Pivots,PivotsTail)
%
% Tries to make as many targetvariables independent by pivoting them with a non-target
% variable. The pivots are stored as T:NT where T is a target variable and NT a non-target
% variable. The non-target variables are marked to be kept during redundancy eliminations.

make_target_indep([],Ps,Ps).
make_target_indep([T|Ts],Ps0,Pst) :-
	(   get_attr(T,clpcd_itf,AttT),
	    arg(1,AttT,CLP),
	    arg(2,AttT,type(Type)),
	    arg(4,AttT,lin([_,_|H])),
	    nontarget(H,Nt)
	->  Ps1 = [T:Nt|Ps0],
	    get_attr(Nt,clpcd_itf,AttN),
	    arg(2,AttN,type(IndAct)),
	    arg(5,AttN,order(Ord)),
	    arg(6,AttN,class(Class)),
	    setarg(11,AttN,keep),
	    pivot(CLP,T,Class,Ord,Type,IndAct)
	;   Ps1 = Ps0
	),
	make_target_indep(Ts,Ps1,Pst).

% nontarget(Hom,Nt)
%
% Finds a nontarget variable in homogene part Hom.
% Hom contains elements of the form l(V*K,OrdV).
% A nontarget variable has no target attribute and no keep_indep attribute.

nontarget([l(V*_,_)|Vs],Nt) :-
	(   get_attr(V,clpcd_itf,Att),
	    arg(9,Att,n),
	    arg(10,Att,n)
	->  Nt = V
	;   nontarget(Vs,Nt)
	).

% drop_dep(Vars)
%
% Does drop_dep_one/1 on each variable in Vars.

drop_dep(Vs) :-
	var(Vs),
	!.
drop_dep([]).
drop_dep([V|Vs]) :-
	drop_dep_one(V),
	drop_dep(Vs).

% drop_dep_one(V)
%
% If V is an unbounded dependent variable that isn't a target variable, shouldn't be kept
% and is not nonzero, drops all linear attributes of V.
% The linear attributes are: type, strictness, linear equation (lin), class and order.

drop_dep_one(V) :-
	get_attr(V,clpcd_itf,Att),
	Att = t(CLP,type(t_none),_,lin(Lin),order(OrdV),_,_,n,n,_,n),
	\+ indep(CLP,Lin,OrdV),
	!,
	setarg(2,Att,n),
	setarg(3,Att,n),
	setarg(4,Att,n),
	setarg(5,Att,n),
	setarg(6,Att,n).
drop_dep_one(_).
