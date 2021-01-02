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


% attribute = t(CLP,type(_),strictness(_),lin(_),order(_),class(_),forward(_),
%		nonzero,target,keep_indep,keep)

:- module(clpcd_itf,
	[
	    dump_linear/3,
	    dump_nonzero/3
	]).

:- use_module(library(neck)).
:- use_module(library(clpcd/class)).
:- use_module(library(clpcd/domain_ops)).
:- use_module(library(clpcd/indep)).
:- use_module(library(clpcd/bv)).
:- use_module(library(clpcd/nf)).
:- use_module(library(clpcd/store)).
:- use_module(library(clpcd/solve)).

% ----------------------------------- dump ------------------------------------

% dump_strict(FilteredStrictness,Nonstrict,Strict,Res)
%
% Unifies Res with either Nonstrict or Strict depending on FilteredStrictness.
% FilteredStrictness is the component of strictness related to the bound: 0
% means nonstrict, 1 means strict upperbound, 2 means strict lowerbound,
% 3 is filtered out to either 1 or 2.

dump_strict(0,Result,_,Result).
dump_strict(1,_,Result,Result).
dump_strict(2,_,Result,Result).

% dump_nz(CLP,V,H,I,Dump,DumpTail)
%
% Returns in Dump a representation of the nonzero constraint of variable V
% which has linear
% equation H + I.

dump_nz(CLP,_,H,I) -->
	{
	    H = [l(_*K,_)|_],
	    div_d(CLP, 1, K, Kr),
	    eval_d(CLP, -Kr*I, I1),
	    mult_hom(H,CLP,Kr,H1),
	    nf2sum(H1,CLP,0,Sum)
	},
	[Sum =\= I1].

% dump_var(Type,Var,I,H,Dump,DumpTail)
%
% Returns in Dump a representation of the linear constraint on variable
% Var which has linear equation H + I and has type Type.

dump_v(t_none,CLP,V,I,H) -->
	!,
	(   {
		H = [l(W*K,_)],
		V == W,
                compare_d(CLP, =, 0, I),
                compare_d(CLP, =, K, 1)
	    }
	->  % indep var
	    []
	;   {nf2sum(H,CLP,I,Sum)},
	    [V = Sum]
	).
dump_v(t_L(L),CLP,V,I,H) -->
	!,
	dump_v(t_l(L),CLP,V,I,H).
% case lowerbound: V >= L or V > L
% say V >= L, and V = K*V1 + ... + I, then K*V1 + ... + I >= L
% and K*V1 + ... >= L-I and V1 + .../K = (L-I)/K
dump_v(t_l(L),CLP,V,I,H) -->
	!,
	{
	    H = [l(_*K,_)|_], % avoid 1 >= 0
	    get_attr(V,clpcd_itf,Att),
	    arg(3,Att,strictness(Strict)),
	    Sm is Strict /\ 2,
            div_d(CLP, 1, K, Kr),
	    eval_d(CLP, Kr*(L - I), Li),
	    mult_hom(H,CLP,Kr,H1),
	    nf2sum(H1,CLP,0,Sum),
	    (   compare_d(CLP, <, 0, K)
	    ->	dump_strict(Sm,Sum >= Li,Sum > Li,Result)
	    ;   dump_strict(Sm,Sum =< Li,Sum < Li,Result)
	    )
	},
	[Result].
dump_v(t_U(U),C,V,I,H) -->
	!,
	dump_v(t_u(U),C,V,I,H).
dump_v(t_u(U),CLP,V,I,H) -->
	!,
	{
	    H = [l(_*K,_)|_], % avoid 0 =< 1
	    get_attr(V,clpcd_itf,Att),
	    arg(3,Att,strictness(Strict)),
	    Sm is Strict /\ 1,
	    div_d(CLP, 1, K, Kr),
	    eval_d(CLP, Kr*(U-I), Ui),
	    mult_hom(H,CLP,Kr,H1),
	    nf2sum(H1,CLP,0,Sum),
	    (   compare_d(CLP, <, 0, K)
	    ->	dump_strict(Sm,Sum =< Ui,Sum < Ui,Result)
	    ;   dump_strict(Sm,Sum >= Ui,Sum > Ui,Result)
	    )
	},
	[Result].
dump_v(t_Lu(L,U),C,V,I,H) -->
	!,
	dump_v(t_l(L),C,V,I,H),
	dump_v(t_u(U),C,V,I,H).
dump_v(t_lU(L,U),C,V,I,H) -->
	!,
	dump_v(t_l(L),C,V,I,H),
	dump_v(t_u(U),C,V,I,H).
dump_v(t_lu(L,U),C,V,I,H) -->
	!,
	dump_v(t_l(L),C,V,I,H),
	dump_v(t_U(U),C,V,I,H).
dump_v(T,_,V,I,H) --> % should not happen
	[V:T:I+H].


% nf2sum(Lin,CLP,Sofar,Term)
%
% Transforms a linear expression into a sum
% (e.g. the expression [5,_,[l(X*2,OrdX),l(Y*-1,OrdY)]] gets transformed into 5 + 2*X - Y)

nf2sum([],_,I,I).
nf2sum([X|Xs],CLP,I,Sum) :-
	(   compare_d(CLP, =, 0, I)
	->  X = l(Var*K,_),
 	    (   % K =:= 1.0
                compare_d(CLP, =, K, 1)
	    ->  hom2sum(Xs,CLP,Var,Sum)
	    ;   % K =:= -1.0
                compare_d(CLP, =, K, -1)
	    ->  hom2sum(Xs,CLP,-Var,Sum)
	    ;	hom2sum(Xs,CLP,K*Var,Sum)
	    )
	;   hom2sum([X|Xs],CLP,I,Sum)
 	).

% hom2sum(Hom,_,Sofar,Term)
%
% Transforms a linear expression into a sum
% this predicate handles all but the first term
% (the first term does not need a concatenation symbol + or -)
% see also nf2sum/3

hom2sum([],_,Term,Term).
hom2sum([l(Var*K,_)|Cs],CLP,Sofar,Term) :-
	(   % K =:= 1.0
            compare_d(CLP, =, K, 1)
	->  Next = Sofar + Var
	;   % K =:= -1.0
            compare_d(CLP, =, K, -1)
	->  Next = Sofar - Var
	;   % K < 0.0
	    compare_d(CLP, <, K, 0)
	->  eval_d(CLP, -K, Ka),
	    Next = Sofar - Ka*Var
	;   Next = Sofar + K*Var
	),
	hom2sum(Cs,CLP,Next,Term).

dump_linear(V) -->
	{
	    get_attr(V,clpcd_itf,Att),
	    arg(1,Att,CLP),
	    arg(2,Att,type(Type)),
	    arg(4,Att,lin(Lin)),
	    !,
	    Lin = [I,_|H]
	},
	(   {
		Type=t_none
	    ;	arg(9,Att,n)
	    }
	->  []
	;   dump_v(t_none,CLP,V,I,H)
	),
	(   {
		Type=t_none,
		arg(9,Att,n) % attribute should not have changed by dump_v...
	    }
	->  % nonzero produces such
	    []
	;   dump_v(Type,CLP,V,I,H)
	).
dump_linear(_) --> [].

dump_nonzero(V) -->
	{
	    get_attr(V,clpcd_itf,Att),
	    arg(1,Att,CLP),
	    arg(4,Att,lin(Lin)),
	    arg(8,Att,nonzero),
	    !,
	    Lin = [I,_|H]
	},
	dump_nz(CLP,V,H,I).
dump_nonzero(_) --> [].

attr_unify_hook(t(CLP,n,n,n,n,n,n,n,_,_,_),Y) :-
	!,
	(   get_attr(Y,clpcd_itf,AttY),
	    \+ arg(1,AttY,CLP)
	->  throw(error(permission_error('mix CLP(CD) variables with',
		'CLP(CD) variables of other subdomain:',Y),context(_)))
	;   true
	).
attr_unify_hook(t(CLP,Ty,St,Li,Or,Cl,_,No,_,_,_),Y) :-
	(   get_attr(Y,clpcd_itf,AttY),
	    \+ arg(1,AttY,CLP)
	->  throw(error(permission_error('mix CLP(CD) variables with',
		'CLP(CD) variables of other subdomain:',Y),context(_)))
	;   true
	),
	do_checks(CLP,Y,Ty,St,Li,Or,Cl,No,Later),
	maplist(call,Later).

do_checks(CLP,Y,Ty,St,Li,Or,Cl,No,Later) :-
    numbers_only(CLP,Y),
    verify_nonzero(No,CLP,Y),
    verify_type(Ty,CLP,St,Y,Later,[]),
    verify_lin(Or,CLP,Cl,Li,Y),
    maplist(call,Later).

% verify_nonzero(Nonzero,CLP,Y)
%
% if Nonzero = nonzero, then verify that Y is not zero
% (if possible, otherwise set Y to be nonzero)

verify_nonzero(nonzero,CLP,Y) :-
    (   var(Y)
    ->  (   get_attr(Y,clpcd_itf,Att)
	->  setarg(8,Att,nonzero)
	;   put_attr(Y,clpcd_itf,t(CLP,n,n,n,n,n,n,nonzero,n,n,n))
	)
    ;   compare_d(CLP, \=, 0, Y)
    ).
verify_nonzero(n,_,_). % X is not nonzero

% verify_type(type(Type),strictness(Strict),Y,[OL|OLT],OLT)
%
% if possible verifies whether Y satisfies the type and strictness of X
% if not possible to verify, then returns the constraints that follow from
% the type and strictness

verify_type(type(Type),CLP,strictness(Strict),Y) -->
    verify_type2(CLP,Y,Type,Strict).
verify_type(n,_,n,_) --> [].

verify_type2(C,Y,TypeX,StrictX) -->
    {var(Y)},
    !,
    verify_type_var(TypeX,C,Y,StrictX).
verify_type2(C,Y,TypeX,StrictX) -->
    {verify_type_nonvar(TypeX,C,Y,StrictX)}.

% verify_type_nonvar(Type,CLP,Nonvar,Strictness)
%
% verifies whether the type and strictness are satisfied with the Nonvar

verify_type_nonvar(t_none,_,_,_).
verify_type_nonvar(t_l(L),C,Value,S) :- ilb(S,C,L,Value).
verify_type_nonvar(t_u(U),C,Value,S) :- iub(S,C,U,Value).
verify_type_nonvar(t_lu(L,U),C,Value,S) :-
    ilb(S,C,L,Value),
    iub(S,C,U,Value).
verify_type_nonvar(t_L(L),C,Value,S) :- ilb(S,C,L,Value).
verify_type_nonvar(t_U(U),C,Value,S) :- iub(S,C,U,Value).
verify_type_nonvar(t_Lu(L,U),C,Value,S) :-
    ilb(S,C,L,Value),
    iub(S,C,U,Value).
verify_type_nonvar(t_lU(L,U),C,Value,S) :-
    ilb(S,C,L,Value),
    iub(S,C,U,Value).

% ilb(Strict,CLP,Lower,Value) & iub(Strict,CLP,Upper,Value)
%
% check whether Value is satisfiable with the given lower/upper bound and
% strictness.
% strictness is encoded as follows:
% 2 = strict lower bound
% 1 = strict upper bound
% 3 = strict lower and upper bound
% 0 = no strict bounds

ilb(S,C,L,V) :-
    between(0, 3, S),
    ( S /\ 2 =:= 0
    ->Op = (=<) % non-strict
    ; Op = (<)  % strict
    ),
    neck,
    compare_d(C, Op, L, V).

iub(S,C,U,V) :-
    between(0, 3, S),
    ( S /\ 1 =:= 0
    ->Op = (=<) % non-strict
    ; Op = (<)  % strict
    ),
    neck,
    compare_d(C, Op, V, U).

%
% Running some goals after X=Y simplifies the coding. It should be possible
% to run the goals here and taking care not to put_atts/2 on X ...
%

% verify_type_var(Type,Var,Strictness,[OutList|OutListTail],OutListTail)
%
% returns the inequalities following from a type and strictness satisfaction
% test with Var

verify_type_var(t_none,_,_,_) --> [].
verify_type_var(t_l(L),C,Y,S) --> llb(S,C,L,Y).
verify_type_var(t_u(U),C,Y,S) --> lub(S,C,U,Y).
verify_type_var(t_lu(L,U),C,Y,S) -->
    llb(S,C,L,Y),
    lub(S,C,U,Y).
verify_type_var(t_L(L),C,Y,S) --> llb(S,C,L,Y).
verify_type_var(t_U(U),C,Y,S) --> lub(S,C,U,Y).
verify_type_var(t_Lu(L,U),C,Y,S) -->
    llb(S,C,L,Y),
    lub(S,C,U,Y).
verify_type_var(t_lU(L,U),C,Y,S) -->
    llb(S,C,L,Y),
    lub(S,C,U,Y).

% llb(Strict,Lower,Value,[OL|OLT],OLT) and lub(Strict,Upper,Value,[OL|OLT],OLT)
%
% returns the inequalities following from the lower and upper bounds and the
% strictness see also lb and ub
llb(S,C,L,V) -->
    {S /\ 2 =:= 0 },
    !,
    [C:{L =< V}].
llb(_,C,L,V) -->
    [C:{L < V}].

lub(S,C,U,V) -->
    {S /\ 1 =:= 0 },
     !,
     [C:{V =< U}].
lub(_,C,U,V) -->
    [C:{V < U}].

%
% We used to drop X from the class/basis to avoid trouble with subsequent
% put_atts/2 on X. Now we could let these dead but harmless updates happen.
% In R however, exported bindings might conflict, e.g. 0 \== 0.0
%
% If X is indep and we do _not_ solve for it, we are in deep shit
% because the ordering is violated.
%
verify_lin(order(OrdX),CLP,class(Class),lin(LinX),Y) :-
    !,
    (   indep(CLP,LinX,OrdX)
    ->  detach_bounds_vlv(CLP,OrdX,LinX,Class,Y,NewLinX),
	% if there were bounds, they are requeued already
	class_drop(Class,Y),
	nf(-Y, CLP, NfY),
	deref(CLP,NfY,LinY),
	add_linear_11(CLP, NewLinX, LinY, Lind),
	(   nf_coeff_of(Lind,OrdX,_)
	->  % X is element of Lind
	    solve_ord_x(CLP, Lind, OrdX, Class)
	;   solve(CLP, Lind)	% X is gone, can safely solve Lind
	)
    ;   class_drop(Class,Y),
	nf(-Y, CLP, NfY),
	deref(CLP,NfY,LinY),
	add_linear_11(CLP, LinX, LinY, Lind),
	solve(CLP, Lind)
    ).
verify_lin(_,_,_,_,_).
