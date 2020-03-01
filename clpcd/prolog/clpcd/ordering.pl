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

:- module(clpcd_ordering,
	  [
              clp_type/2,
	      get_or_add_class/2,
	      ordering/1,
              var_intern/4,
	      arrangement/2
	  ]).

:- use_module(library(clpcd/class)).
:- use_module(library(clpcd/combine)).
:- use_module(library(ugraphs)).

ordering(X) :-
	var(X),
	!,
	fail.
ordering(A>B) :-
	!,
	ordering(B<A).
ordering(A<B) :-
	join_class([A,B],Class),
	class_get_prio(Class,Ga),
	!,
	add_edges([],[A-B],Gb),
	combine(Ga,Gb,Gc),
	class_put_prio(Class,Gc).
ordering(Pb) :-
	Pb = [_|Xs],
	join_class(Pb,Class),
	class_get_prio(Class,Ga),
	!,
	(   Xs = [],
	    add_vertices([],Pb,Gb)
	;   Xs=[_|_],
	    gen_edges(Pb,Es,[]),
	    add_edges([],Es,Gb)
	),
	combine(Ga,Gb,Gc),
	class_put_prio(Class,Gc).
ordering(_).

arrangement(Class,Arr) :-
	class_get_prio(Class,G),
	normalize(G,Gn),
	top_sort(Gn,Arr),
	!.
arrangement(_,_) :- throw(unsatisfiable_ordering).

% TODO
%
%

var_intern(CLP,Type,Var,Strict) :-
        var_intern(CLP,Type,Var,Strict,_Class).

var_intern(CLP,Type,Var,Strict,Class) :-
	put_attr(Var,clpcd_itf,t(CLP,type(Type),strictness(Strict),
	    lin([0,0,l(Var*1,Ord)]),order(Ord),n,n,n,n,n,n)),
	get_or_add_class(Var,Class).

% TODO
%
%

var_intern(_CLP,Var,Class) :-	% for ordered/1 but otherwise free vars
	get_attr(Var,clpcd_itf,Att),
	arg(2,Att,type(_)),
	arg(4,Att,lin(_)),
	!,
	get_or_add_class(Var,Class).
var_intern(CLP,Var,Class) :-
        var_intern(CLP,t_none,Var,0,Class).

% get_or_add_class(X,Class)
%
% Returns in Class the class of X if X has one, or a new class where X now
% belongs to if X didn't have one.

get_or_add_class(X,Class) :-
	get_attr(X,clpcd_itf,Att),
	arg(1,Att,CLP),
	(   arg(6,Att,class(ClassX))
	->  ClassX = Class
	;   setarg(6,Att,class(Class)),
	    class_new(Class,CLP,[X|Tail],Tail,[])
	).

join_class([],_).
join_class([X|Xs],Class) :-
	(   var(X)
	->  clp_type(X,CLP),
            var_intern(CLP, X, Class)
	;   true
	),
	join_class(Xs,Class).

clp_type(Var,Type) :-
	(   get_attr(Var,clpcd_itf,Att)
	->  arg(1,Att,Type)
	;   get_attr(Var,clpcd_geler,Att)
	->  arg(1,Att,Type)
	).

gen_edges([]) --> [].
gen_edges([X|Xs]) -->
	gen_edges(Xs,X),
	gen_edges(Xs).

gen_edges([],_) --> [].
gen_edges([Y|Ys],X) -->
	[X-Y],
	gen_edges(Ys,X).

		 /*******************************
		 *	       SANDBOX		*
		 *******************************/
:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(clpcd_ordering:ordering(_)).
sandbox:safe_primitive(clpcd_ordering:clp_type(_,_)).
