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

:- module(clpcd_attributes,
	[
	    project_attributes/2
	]).

:- use_module(library(clpcd/fourmotz)).
:- use_module(library(clpcd/geler)).
:- use_module(library(clpcd/project)).
:- use_module(library(clpcd/store)).
:- use_module(library(clpcd/redund)).
:- use_module(library(clpcd/solve)).
:- use_module(library(clpcd/class)).
:- use_module(library(clpcd/ordering)).

%:- public project_attributes/2. 		% xref.pl

%
% interface predicate
%
% May be destructive (either acts on a copy or in a failure loop)
%
project_attributes(TargetVars,Cvas) :-
	sort(TargetVars,Tvs),		% duplicates ?
	sort(Cvas,Avs),			% duplicates ?
	get_clp(TargetVars,CLP),
	(   nonvar(CLP)
	->  mark_target(Tvs),
	    project_nonlin(Tvs,Avs,NlReachable),
	    (   Tvs == []
	    ->  drop_lin_atts(Avs)
	    ;   redundancy_vars(Avs),		% removes redundant bounds (redund.pl)
		make_target_indep(Tvs,Pivots),	% pivot partners are marked to be kept during elim.	
		mark_target(NlReachable),	% after make_indep to express priority
		drop_dep(Avs),
		fm_elim(CLP,Avs,Tvs,Pivots),
		impose_ordering(Avs)
	    )
	;   true
	).

get_clp([],_).
get_clp([H|T],CLP) :-
	(   get_attr(H,clpcd_itf,Att)
	->  arg(1,Att,CLP)
	;   true
	),
	get_clp(T,CLP).

% mark_target(Vars)
%
% Marks the variables in Vars as target variables.

mark_target([]).
mark_target([V|Vs]) :-
	(   get_attr(V,clpcd_itf,Att)
	->  setarg(9,Att,target)
	;   true
	),
	mark_target(Vs).

% drop_lin_atts(Vs)
%
% Removes the linear attributes of the variables in Vs.
% The linear attributes are type, strictness, linear equation (lin), order and class.

drop_lin_atts([]).
drop_lin_atts([V|Vs]) :-
	get_attr(V,clpcd_itf,Att),
	setarg(2,Att,n),
	setarg(3,Att,n),
	setarg(4,Att,n),
	setarg(5,Att,n),
	setarg(6,Att,n),
	drop_lin_atts(Vs).

impose_ordering(Cvas) :-
	systems(Cvas,[],Sys),
	impose_ordering_sys(Sys).

impose_ordering_sys([]).
impose_ordering_sys([S|Ss]) :-
	arrangement(S,Arr),	% ordering.pl
	arrange(Arr,S),
	impose_ordering_sys(Ss).

arrange([],_).
arrange(Arr,S) :-
	Arr = [_|_],
	class_allvars(S,All),
	order(Arr,1,N),
	order(All,N,_),
	renorm_all(All),
	arrange_pivot(All).

order(Xs,N,M) :-
	var(Xs),
	!,
	N = M.
order([],N,N).
order([X|Xs],N,M) :-
	(   get_attr(X,clpcd_itf,Att),
	    arg(5,Att,order(O)),
	    var(O)
	->  O = N,
	    N1 is N+1,
	    order(Xs,N1,M)
	;   order(Xs,N,M)
	).

% renorm_all(Vars)
%
% Renormalizes all linear equations of the variables in difference list Vars to reflect
% their new ordering.

renorm_all(Xs) :-
	var(Xs),
	!.
renorm_all([X|Xs]) :-
	(   get_attr(X,clpcd_itf,Att),
	    arg(1,Att,CLP),
	    arg(4,Att,lin(Lin))
	->  renormalize(CLP,Lin,New),
	    setarg(4,Att,lin(New)),
	    renorm_all(Xs)
	;   renorm_all(Xs)
	).

% arrange_pivot(Vars)
%
% If variable X of Vars has type t_none and has a higher order than the first element of
% its linear equation, then it is pivoted with that element.

arrange_pivot(Xs) :-
	var(Xs),
	!.
arrange_pivot([X|Xs]) :-
	(   get_attr(X,clpcd_itf,AttX),
	    %arg(8,AttX,n), % not for nonzero
	    arg(1,AttX,CLP),
	    arg(2,AttX,type(t_none)),
	    arg(4,AttX,lin(Lin)),
	    arg(5,AttX,order(OrdX)),
	    Lin = [_,_,l(Y*_,_)|_],
	    get_attr(Y,clpcd_itf,AttY),
	    arg(2,AttY,type(IndAct)),
	    arg(5,AttY,order(OrdY)),
	    arg(6,AttY,class(Class)),
	    compare(>,OrdY,OrdX)
	->  pivot(CLP,X,Class,OrdY,t_none,IndAct),
	    arrange_pivot(Xs)
	;   arrange_pivot(Xs)
	).
