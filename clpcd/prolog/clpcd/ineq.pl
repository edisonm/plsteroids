/*

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


:- module(clpcd_ineq,
	[
	    ineq/5,
	    ineq_one/5,
	    ineq_one_n_n_0/2,
	    ineq_one_n_p_0/2,
	    ineq_one_s_n_0/2,
	    ineq_one_s_p_0/2
	]).

:- use_module(library(clpcd/domain_ops)).
:- use_module(library(clpcd/ordering)).
:- use_module(library(clpcd/bv)).
:- use_module(library(clpcd/store)).

% ineq(H,I,Nf,Strictness)
%
% Solves the inequality Nf < 0 or Nf =< 0 where Nf is in normal form
% and H and I are the homogene and inhomogene parts of Nf.

ineq([], CLP, I, _, Strictness) :- ineq_ground(Strictness, CLP, I).
ineq([v(K,[X^1])|Tail], CLP, I, Lin, Strictness) :-
	ineq_cases(Tail, CLP, I, Lin, Strictness, X, K).

ineq_cases([], CLP, I, _, Strictness, X, K) :-	% K*X + I < 0 or K*X + I =< 0
	ineq_one(Strictness, CLP, X, K, I).
ineq_cases([_|_], CLP, _, Lin, Strictness, _, _) :-
	deref(CLP, Lin, Lind),	% Id+Hd =< 0
	Lind = [Inhom,_|Hom],
	ineq_more(Hom, CLP, Inhom, Lind, Strictness).

% ineq_ground(Strictness,I)
%
% Checks whether a grounded inequality I < 0 or I =< 0 is satisfied.

ineq_ground(strict,    CLP, I) :- compare_d(CLP,  <, I, 0).
ineq_ground(nonstrict, CLP, I) :- compare_d(CLP, =<, I, 0).

% ineq_one(Strictness,X,K,I)
%
% Solves the inequality K*X + I < 0 or K*X + I =< 0

ineq_one(strict, CLP, X, K, I) :-
	(   K > 0
	->  (   I =:= 0
	    ->  ineq_one_s_p_0(CLP, X)	% K*X < 0, K > 0 => X < 0
	    ;   div_d(CLP, I, K, Inhom),
		ineq_one_s_p_i(CLP, X, Inhom)	% K*X + I < 0, K > 0 => X + I/K < 0
	    )
	;   (   I =:= 0
	    ->  ineq_one_s_n_0(CLP, X)	% K*X < 0, K < 0 => -X < 0
	    ;   div_d(CLP, -I, K, Inhom),
		ineq_one_s_n_i(CLP, X, Inhom)	% K*X + I < 0, K < 0 => -X - I/K < 0
	    )
	).
ineq_one(nonstrict, CLP, X, K, I) :-
	(   K > 0
	->  (   I =:= 0
	    ->  ineq_one_n_p_0(CLP, X)	% K*X =< 0, K > 0 => X =< 0
	    ;   div_d(CLP, I, K, Inhom),
		ineq_one_n_p_i(CLP, X, Inhom)	% K*X + I =< 0, K > 0 => X + I/K =< 0
	    )
	;   (   I =:= 0
	    ->  ineq_one_n_n_0(CLP, X)	% K*X =< 0, K < 0 => -X =< 0
	    ;   div_d(CLP, -I, K, Inhom),
		ineq_one_n_n_i(CLP, X, Inhom)	% K*X + I =< 0, K < 0 => -X - I/K =< 0
	    )
	).

% --------------------------- strict ----------------------------

% ineq_one_s_p_0(CLP, X)
%
% Solves the inequality X < 0

ineq_one_s_p_0(CLP, X) :-
	get_attr(X,clpcd_itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,	% old variable, this is deref
	(   \+ arg(1,Att,CLP)
	->  throw(error(permission_error('mix CLPCD variables with',
		'variables of a different domain:',X),context(_)))
	;   ineq_one_old_s_p_0(OrdX, CLP, X, Ix)
	).
ineq_one_s_p_0(CLP, X) :-	% new variable, nothing depends on it
	var_intern(CLP,t_u(0),X,1). % put a strict inactive upperbound on the variable

% ineq_one_s_n_0(CLP, X)
%
% Solves the inequality X > 0

ineq_one_s_n_0(CLP, X) :-
	get_attr(X,clpcd_itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,
	(   \+ arg(1,Att,CLP)
	->  throw(error(permission_error('mix CLPCD variables with',
		'variables of a different domain:',X),context(_)))
	;   ineq_one_old_s_n_0(OrdX, CLP, X, Ix)
	).
ineq_one_s_n_0(CLP, X) :-
	var_intern(CLP,t_l(0),X,2). % puts a strict inactive lowerbound on the variable

% ineq_one_s_p_i(X,I)
%
% Solves the inequality X < -I

ineq_one_s_p_i(CLP, X, I) :-
	get_attr(X,clpcd_itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,
	(   \+ arg(1,Att,CLP)
	->  throw(error(permission_error('mix CLPCD variables with',
		'variables of a different domain:',X),context(_)))
	;   ineq_one_old_s_p_i(OrdX, CLP, I, X, Ix)
	).
ineq_one_s_p_i(CLP, X, I) :-
	Bound is -I,
	var_intern(CLP,t_u(Bound),X,1). % puts a strict inactive upperbound on the variable

% ineq_one_s_n_i(CLP,X,I)
%
% Solves the inequality X > I

ineq_one_s_n_i(CLP, X, I) :-
	get_attr(X,clpcd_itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,
	(   \+ arg(1,Att,CLP)
	->  throw(error(permission_error('mix CLPCD variables with',
		'variables of a different domain:',X),context(_)))
	;   ineq_one_old_s_n_i(OrdX, CLP, I, X, Ix)
	).
ineq_one_s_n_i(CLP, X, I) :- var_intern(CLP,t_l(I),X,2). % puts a strict inactive lowerbound on the variable

% ineq_one_old_s_p_0(Hom,CLP,X,Inhom)
%
% Solves the inequality X < 0 where X has linear equation Hom + Inhom

ineq_one_old_s_p_0([], CLP, _, Ix) :- compare_d(CLP, <, Ix, 0). % X = I: Ix < 0
ineq_one_old_s_p_0([l(Y*Ky,_)|Tail], CLP, X, Ix) :-
	(   Tail = [] % X = K*Y + I
	->  div_d(CLP, -Ix, Ky, Bound),
	    update_indep(strict, CLP, Y, Ky, Bound)	% X < 0, X = K*Y + I => Y < -I/K or Y > -I/K (depending on K)
	;   Tail = [_|_]
	->  get_attr(X,clpcd_itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udus(Type, CLP, X, Lin, 0, Old)	% update strict upperbound
	).

% ineq_one_old_s_p_0(Hom,X,Inhom)
%
% Solves the inequality X > 0 where X has linear equation Hom + Inhom

ineq_one_old_s_n_0([], CLP, _, Ix) :- compare_d(CLP, >, Ix, 0). % X = I: Ix > 0
ineq_one_old_s_n_0([l(Y*Ky,_)|Tail], CLP, X, Ix) :-
	(   Tail = []	% X = K*Y + I
	->  Coeff is -Ky,
	    div_d(CLP, Ix, Coeff, Bound),
	    update_indep(strict, CLP, Y, Coeff, Bound)
	;   Tail = [_|_]
	->  get_attr(X,clpcd_itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udls(Type, CLP, X, Lin, 0, Old)	% update strict lowerbound
	).

% ineq_one_old_s_p_i(Hom,C,X,Inhom)
%
% Solves the inequality X + C < 0 where X has linear equation Hom + Inhom

ineq_one_old_s_p_i([], CLP, I, _, Ix) :- compare_d(CLP, <, I, -Ix). % X = I
ineq_one_old_s_p_i([l(Y*Ky,_)|Tail], CLP, I, X, Ix) :-
	(   Tail = []	% X = K*Y + I
	->  div_d(CLP, -(Ix + I), Ky, Bound),
	    update_indep(strict, CLP, Y, Ky, Bound)
	;   Tail = [_|_]
	->  Bound is -I,
	    get_attr(X,clpcd_itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udus(Type, CLP, X, Lin, Bound, Old)	% update strict upperbound
	).

% ineq_one_old_s_n_i(Hom,C,X,Inhom)
%
% Solves the inequality X  - C > 0 where X has linear equation Hom + Inhom

ineq_one_old_s_n_i([], CLP, I, _, Ix) :- compare_d(CLP, <, I, Ix). % X = I
ineq_one_old_s_n_i([l(Y*Ky,_)|Tail], CLP, I, X, Ix) :-
	(   Tail = []	% X = K*Y + I
	->  Coeff is -Ky,
	    div_d(CLP, Ix - I, Coeff, Bound),
	    update_indep(strict, CLP, Y, Coeff, Bound)
	;   Tail = [_|_]
	->  get_attr(X,clpcd_itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udls(Type, CLP, X, Lin, I, Old)	% update strict lowerbound
	).

% -------------------------- nonstrict --------------------------

% ineq_one_n_p_0(X)
%
% Solves the inequality X =< 0

ineq_one_n_p_0(CLP, X) :-
	get_attr(X,clpcd_itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!, % old variable, this is deref
	(   \+ arg(1,Att,CLP)
	->  throw(error(permission_error('mix CLPCD variables with',
		'variables of a different domain:',X),context(_)))
	;   ineq_one_old_n_p_0(OrdX, CLP, X, Ix)
	).
ineq_one_n_p_0(CLP, X) :-	% new variable, nothing depends on it
	var_intern(CLP,t_u(0),X,0).	% nonstrict upperbound

% ineq_one_n_n_0(X)
%
% Solves the inequality X >= 0

ineq_one_n_n_0(CLP, X) :-
	get_attr(X,clpcd_itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,
	(   \+ arg(1,Att,CLP)
	->  throw(error(permission_error('mix CLPCD variables with',
		'variables of a different domain:',X),context(_)))
	;   ineq_one_old_n_n_0(OrdX, CLP, X, Ix)
	).
ineq_one_n_n_0(CLP, X) :-
	var_intern(CLP,t_l(0),X,0).	% nonstrict lowerbound

% ineq_one_n_p_i(X,I)
%
% Solves the inequality X =< -I

ineq_one_n_p_i(CLP, X, I) :-
	get_attr(X,clpcd_itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,
	(   \+ arg(1,Att,CLP)
	->  throw(error(permission_error('mix CLP(Q) variables with',
		'variables of a different domain:',X),context(_)))
	;   ineq_one_old_n_p_i(OrdX, CLP, I, X, Ix)
	).
ineq_one_n_p_i(CLP, X, I) :-
	Bound is -I,
	var_intern(CLP,t_u(Bound),X,0).	% nonstrict upperbound

% ineq_one_n_n_i(CLP,X,I)
%
% Solves the inequality X >= I

ineq_one_n_n_i(CLP, X, I) :-
	get_attr(X,clpcd_itf,Att),
	arg(4,Att,lin([Ix,_|OrdX])),
	!,
	(   \+ arg(1,Att,CLP)
	->  throw(error(permission_error('mix CLP(Q) variables with',
		'CLP(R) variables:',X),context(_)))
	;   ineq_one_old_n_n_i(OrdX, CLP, I, X, Ix)
	).
ineq_one_n_n_i(CLP, X, I) :-
	var_intern(CLP,t_l(I),X,0).	% nonstrict lowerbound

% ineq_one_old_n_p_0(Hom,X,Inhom)
%
% Solves the inequality X =< 0 where X has linear equation Hom + Inhom

ineq_one_old_n_p_0([], CLP, _, Ix) :- compare_d(CLP, =<, Ix, 0). % X =I
ineq_one_old_n_p_0([l(Y*Ky,_)|Tail], CLP, X, Ix) :-
	(   Tail = []	%  X = K*Y + I
	->  div_d(CLP, -Ix, Ky, Bound),
	    update_indep(nonstrict, CLP, Y, Ky, Bound)
	;   Tail = [_|_]
	->  get_attr(X,clpcd_itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udu(Type, CLP, X, Lin, 0, Old)	% update nonstrict upperbound
	).

% ineq_one_old_n_n_0(Hom,X,Inhom)
%
% Solves the inequality X >= 0 where X has linear equation Hom + Inhom

ineq_one_old_n_n_0([], CLP, _, Ix) :- compare_d(CLP, >=, Ix, 0). % X = I
ineq_one_old_n_n_0([l(Y*Ky,_)|Tail], CLP, X, Ix) :-
	(   Tail = []	% X = K*Y + I
	->  Coeff is -Ky,
	    div_d(CLP, Ix, Coeff, Bound),
	    update_indep(nonstrict, CLP, Y, Coeff, Bound)
	;   Tail = [_|_]
	->  get_attr(X,clpcd_itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udl(Type, CLP, X, Lin, 0, Old)	% update nonstrict lowerbound
	).

% ineq_one_old_n_p_i(Hom,C,X,Inhom)
%
% Solves the inequality X  + C =< 0 where X has linear equation Hom + Inhom

ineq_one_old_n_p_i([], CLP, I, _, Ix) :- compare_d(CLP, =<, I, -Ix).	% X = I
ineq_one_old_n_p_i([l(Y*Ky,_)|Tail], CLP, I, X, Ix) :-
	(   Tail = []	% X = K*Y + I
	->  div_d(CLP, -(Ix + I), Ky, Bound),
	    update_indep(nonstrict, CLP, Y, Ky, Bound)
	;   Tail = [_|_]
	->  Bound is -I,
	    get_attr(X,clpcd_itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udu(Type, CLP, X, Lin, Bound, Old)	% update nonstrict upperbound
	).

% ineq_one_old_n_n_i(Hom,C,X,Inhom)
%
% Solves the inequality X  - C >= 0 where X has linear equation Hom + Inhom

ineq_one_old_n_n_i([], CLP, I, _, Ix) :- compare_d(CLP, =<, I, Ix). % X = I
ineq_one_old_n_n_i([l(Y*Ky,_)|Tail], CLP, I, X, Ix) :-
	(   Tail = []
	->  Coeff is -Ky,
	    div_d(CLP, Ix - I, Coeff, Bound),
	    update_indep(nonstrict, CLP, Y, Coeff, Bound)
	;   Tail = [_|_]
	->  get_attr(X,clpcd_itf,Att),
	    arg(2,Att,type(Type)),
	    arg(3,Att,strictness(Old)),
	    arg(4,Att,lin(Lin)),
	    udl(Type, CLP, X, Lin, I, Old)
	).

% ---------------------------------------------------------------

% ineq_more(Hom,Inhom,Lin,Strictness)
%
% Solves the inequality Lin < 0 or Lin =< 0 with Lin = Hom + Inhom

ineq_more([], CLP, I, _, Strictness) :- ineq_ground(Strictness,CLP,I).	% I < 0 or I =< 0
ineq_more([l(X*K,_)|Tail], CLP, Id, Lind, Strictness) :-
	(   Tail = []
	->  % X*K < Id or X*K =< Id
	    % one var: update bound instead of slack introduction
	    get_or_add_class(X,_),	% makes sure X belongs to a class
	    div_d(CLP, -Id, K, Bound),
	    update_indep(Strictness, CLP, X, K, Bound)	% new bound
	;   Tail = [_|_]
	->  ineq_more(Strictness, CLP, Lind)
	).

% ineq_more(Strictness,CLP,Lin)
%
% Solves the inequality Lin < 0 or Lin =< 0

ineq_more(strict, CLP, Lind) :-
	(   unconstrained(Lind,U,K,Rest)
	->  % never fails, no implied value
	    % Lind < 0 => Rest < -K*U where U has no bounds
	    var_intern(CLP,t_l(0),S,2),	% create slack variable S
	    get_attr(S,clpcd_itf,AttS),
	    arg(5,AttS,order(OrdS)),
	    div_d(CLP, -1, K, Ki),
	    add_linear_ff(CLP, Rest, Ki, [0,0,l(S*1,OrdS)], Ki, LinU),	% U = (-1/K)*Rest + (-1/K)*S
	    LinU = [_,_|Hu],
	    get_or_add_class(U,Class),
	    same_class(Hu,Class),	% put all variables of new lin. eq. of U in the same class
	    get_attr(U,clpcd_itf,AttU),
	    arg(5,AttU,order(OrdU)),
	    arg(6,AttU,class(ClassU)),
	    backsubst(CLP, ClassU, OrdU, LinU)	% substitute U by new lin. eq. everywhere in the class
	;   var_with_def_intern(t_u(0), CLP, S, Lind, 1),	% Lind < 0 => Lind = S with S < 0
	    basis_add(S,_),			% adds S to the basis
	    determine_active_dec(Lind),		% activate bounds
	    reconsider(CLP, S)			% reconsider basis
	).
ineq_more(nonstrict, CLP, Lind) :-
	(   unconstrained(Lind,U,K,Rest)
	->  % never fails, no implied value
	    % Lind =< 0 => Rest =< -K*U where U has no bounds
	    var_intern(CLP,t_l(0),S,0),	% create slack variable S
	    div_d(CLP, -1, K, Ki),
	    get_attr(S,clpcd_itf,AttS),
	    arg(5,AttS,order(OrdS)),
	    add_linear_ff(CLP, Rest, Ki, [0,0,l(S*1,OrdS)], Ki, LinU),	% U = (-1K)*Rest + (-1/K)*S
	    LinU = [_,_|Hu],
	    get_or_add_class(U,Class),
	    same_class(Hu,Class),	% put all variables of new lin. eq of U in the same class
	    get_attr(U,clpcd_itf,AttU),
	    arg(5,AttU,order(OrdU)),
	    arg(6,AttU,class(ClassU)),
	    backsubst(CLP, ClassU, OrdU, LinU)	% substitute U by new lin. eq. everywhere in the class
	;   % all variables are constrained
	    var_with_def_intern(t_u(0), CLP, S, Lind, 0),	% Lind =< 0 => Lind = S with S =< 0
	    basis_add(S,_),				% adds S to the basis
	    determine_active_dec(Lind),
	    reconsider(CLP, S)
	).


% update_indep(Strictness,X,K,Bound)
%
% Updates the bound of independent variable X where X < Bound or X =< Bound
% or X > Bound or X >= Bound, depending on Strictness and K.

update_indep(strict, CLP, X, K, Bound) :-
	get_attr(X,clpcd_itf,Att),
	arg(2,Att,type(Type)),
	arg(3,Att,strictness(Old)),
	arg(4,Att,lin(Lin)),
	(   K < 0
	->  uils(Type, CLP, X, Lin, Bound, Old)	% update independent lowerbound strict
	;   uius(Type, CLP, X, Lin, Bound, Old)	% update independent upperbound strict
	).
update_indep(nonstrict, CLP, X, K, Bound) :-
	get_attr(X,clpcd_itf,Att),
	arg(2,Att,type(Type)),
	arg(3,Att,strictness(Old)),
	arg(4,Att,lin(Lin)),
	(   K < 0
	->  uil(Type, CLP, X, Lin, Bound, Old)	% update independent lowerbound nonstrict
	;   uiu(Type, CLP, X, Lin, Bound, Old)	% update independent upperbound nonstrict
	).


% ---------------------------------------------------------------------------------------

%
% Update a bound on a var xi
%
%   a) independent variable
%
%	a1) update inactive bound: done
%
%	a2) update active bound:
%	    Determine [lu]b including most constraining row R
%	      If we are within: done
%	    else pivot(R,xi) and introduce bound via (b)
%
%	a3) introduce a bound on an unconstrained var:
%	    All vars that depend on xi are unconstrained (invariant) ->
%	      the bound cannot invalidate any Lhs
%
%   b) dependent variable
%
%	repair upper or lower (maybe just swap with an unconstrained var from Rhs)
%

%
% Sign = 1,0,-1 means inside,at,outside
%

% Read following predicates as update (dependent/independent) (lowerbound/upperbound) (strict)

% udl(Type,X,Lin,Bound,Strict)
%
% Updates lower bound of dependent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new non-strict
% bound Bound.

udl(t_none, CLP, X, Lin, Bound, _Sold) :-
	get_attr(X,clpcd_itf,AttX),
	arg(5,AttX,order(Ord)),
	setarg(2,AttX,type(t_l(Bound))),
	setarg(3,AttX,strictness(0)),
	(   unconstrained(Lin,Uc,Kuc,Rest)
	->  % X = Lin => -1/K*Rest + 1/K*X = U where U has no bounds
	    div_d(CLP, -1, Kuc, Ki),
	    add_linear_ff(CLP, Rest, Ki, [0,0,l(X* -1,Ord)], Ki, LinU),
	    get_attr(Uc,clpcd_itf,AttU),
	    arg(5,AttU,order(OrdU)),
	    arg(6,AttU,class(Class)),
	    backsubst(CLP, Class, OrdU, LinU)
	;   % no unconstrained variables in Lin: make X part of basis and reconsider
	    basis_add(X,_),
	    determine_active_inc(Lin),
	    reconsider(CLP, X)
	).
udl(t_l(L), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, >, Bound, L)
	->  % new bound is larger than old one: use new and reconsider basis
	    Strict is Sold /\ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_l(Bound))),
	    setarg(3,Att,strictness(Strict)),
	    reconsider_lower(CLP, X, Lin, Bound)	% makes sure that Lin still satisfies lowerbound Bound
	;   true	% new bound is equal to old one, new one is nonstrict: keep old
	).

udl(t_u(U), CLP, X, Lin, Bound, _Sold) :-
	(   compare_d(CLP, <, Bound, U)
	->  % new bound is smaller than upperbound: add new and reconsider basis
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_lu(Bound,U))),
	    reconsider_lower(CLP, X, Lin, Bound)	% makes sure that Lin still satisfies lowerbound Bound
	;   compare_d(CLP, =, Bound, U),
            solve_bound(CLP, Lin, Bound)	% new bound is equal to upperbound: solve
	).
udl(t_lu(L,U), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, >, Bound, L)
	->  % larger than lowerbound: check upperbound
	    (   compare_d(CLP, <, Bound, U)
	    ->  % smaller than upperbound: use new and reconsider basis
		Strict is Sold /\ 1,
		get_attr(X,clpcd_itf,Att),
		setarg(2,Att,type(t_lu(Bound,U))),
		setarg(3,Att,strictness(Strict)),
		reconsider_lower(CLP, X, Lin, Bound)
	    ;   compare_d(CLP, =, Bound, U),
                % equal to upperbound: if strictness matches => solve
		Sold /\ 1 =:= 0,
		solve_bound(CLP, Lin, Bound)
	    )
	;   true	% equal to lowerbound and nonstrict: keep
	).

% udls(Type,X,Lin,Bound,Strict)
%
% Updates lower bound of dependent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new strict
% bound Bound.

udls(t_none, CLP, X, Lin, Bound, _Sold) :-
	get_attr(X,clpcd_itf,AttX),
	arg(5,AttX,order(Ord)),
	setarg(2,AttX,type(t_l(Bound))),
	setarg(3,AttX,strictness(2)),
	(   unconstrained(Lin,Uc,Kuc,Rest)
	->  % X = Lin => U = -1/K*Rest + 1/K*X with U an unconstrained variable
	    div_d(CLP, -1, Kuc, Ki),
	    add_linear_ff(CLP, Rest, Ki, [0,0,l(X* -1,Ord)], Ki, LinU),
	    get_attr(Uc,clpcd_itf,AttU),
	    arg(5,AttU,order(OrdU)),
	    arg(6,AttU,class(Class)),
	    backsubst(CLP, Class, OrdU, LinU)
	;   % no unconstrained variables: add X to basis and reconsider basis
	    basis_add(X,_),
	    determine_active_inc(Lin),
	    reconsider(CLP, X)
	).
udls(t_l(L), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, <, Bound, L)
	->  true	% smaller than lowerbound: keep
	;   compare_d(CLP, >, Bound, L)
	->  % larger than lowerbound: use new and reconsider basis
	    Strict is Sold \/ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_l(Bound))),
	    setarg(3,Att,strictness(Strict)),
	    reconsider_lower(CLP, X, Lin, Bound)
	;   % equal to lowerbound: check strictness
	    Strict is Sold \/ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
udls(t_u(U), CLP, X, Lin, Bound, Sold) :-
	compare_d(CLP, <, Bound, U),	% smaller than upperbound: set new bound
	Strict is Sold \/ 2,
	get_attr(X,clpcd_itf,Att),
	setarg(2,Att,type(t_lu(Bound,U))),
	setarg(3,Att,strictness(Strict)),
	reconsider_lower(CLP, X, Lin, Bound).
udls(t_lu(L,U), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, <, Bound, L)
	->  true	% smaller than lowerbound: keep
	;   compare_d(CLP, >, Bound, L)
	->  % larger than lowerbound: check upperbound and possibly use new and reconsider basis
	    compare_d(CLP, <, Bound, U),
	    Strict is Sold \/ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_lu(Bound,U))),
	    setarg(3,Att,strictness(Strict)),
	    reconsider_lower(CLP, X, Lin, Bound)
	;   % equal to lowerbound: put new strictness
	    Strict is Sold \/ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).

% udu(Type,X,Lin,Bound,Strict)
%
% Updates upper bound of dependent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new non-strict
% bound Bound.

udu(t_none, CLP, X, Lin, Bound, _Sold) :-
	get_attr(X,clpcd_itf,AttX),
	arg(5,AttX,order(Ord)),
	setarg(2,AttX,type(t_u(Bound))),
	setarg(3,AttX,strictness(0)),
	(   unconstrained(Lin,Uc,Kuc,Rest)
	->  % X = Lin => U = -1/K*Rest + 1/K*X with U an unconstrained variable
	    div_d(CLP, -1, Kuc, Ki),
	    add_linear_ff(CLP, Rest, Ki, [0,0,l(X* -1,Ord)], Ki, LinU),
	    get_attr(Uc,clpcd_itf,AttU),
	    arg(5,AttU,order(OrdU)),
	    arg(6,AttU,class(Class)),
	    backsubst(CLP, Class, OrdU, LinU)
	;   % no unconstrained variables: add X to basis and reconsider basis
	    basis_add(X,_),
	    determine_active_dec(Lin),	% try to lower R
	    reconsider(CLP, X)
	).
udu(t_u(U), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, <, Bound, U)
	->  % smaller than upperbound: update and reconsider basis
	    Strict is Sold /\ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_u(Bound))),
	    setarg(3,Att,strictness(Strict)),
	    reconsider_upper(CLP, X, Lin, Bound)
	;   true	% equal to upperbound and nonstrict: keep
	).
udu(t_l(L), CLP, X, Lin, Bound, _Sold) :-
	(   compare_d(CLP, >, Bound, L)
	->  % larger than lowerbound: use new and reconsider basis
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_lu(L,Bound))),
	    reconsider_upper(CLP, X, Lin, Bound)
	;   compare_d(CLP, =, Bound, L),
            solve_bound(CLP, Lin, Bound)	% equal to lowerbound: solve
	).
udu(t_lu(L,U), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, <, Bound, U)
	->  % smaller than upperbound: check lowerbound
	    (   compare_d(CLP, >, Bound, L)
	    ->  % larger than lowerbound: update and reconsider basis
		Strict is Sold /\ 2,
		get_attr(X,clpcd_itf,Att),
		setarg(2,Att,type(t_lu(L,Bound))),
		setarg(3,Att,strictness(Strict)),
		reconsider_upper(CLP, X, Lin, Bound)
	    ;   compare_d(CLP, =, Bound, L),
                % equal to lowerbound: check strictness and possibly solve
		Sold /\ 2 =:= 0,
		solve_bound(CLP, Lin, Bound)
	    )
	;   true	% equal to upperbound and nonstrict: keep
	).

% udus(Type,X,Lin,Bound,Strict)
%
% Updates upper bound of dependent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new strict
% bound Bound.

udus(t_none, CLP, X, Lin, Bound, _Sold) :-
	get_attr(X,clpcd_itf,AttX),
	arg(5,AttX,order(Ord)),
	setarg(2,AttX,type(t_u(Bound))),
	setarg(3,AttX,strictness(1)),
	(   unconstrained(Lin,Uc,Kuc,Rest)
	->   % X = Lin => U = -1/K*Rest + 1/K*X with U an unconstrained variable
	    div_d(CLP, -1, Kuc, Ki),
	    add_linear_ff(CLP, Rest, Ki, [0,0,l(X* -1,Ord)], Ki, LinU),
	    get_attr(Uc,clpcd_itf,AttU),
	    arg(5,AttU,order(OrdU)),
	    arg(6,AttU,class(Class)),
	    backsubst(CLP, Class, OrdU, LinU)
	;   % no unconstrained variables: add X to basis and reconsider basis
	    basis_add(X,_),
	    determine_active_dec(Lin),
	    reconsider(CLP, X)
	).
udus(t_u(U), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, <, U, Bound)
	->  true	% larger than upperbound: keep
	;   compare_d(CLP, <, Bound, U)
	->  % smaller than upperbound: update bound and reconsider basis
	    Strict is Sold \/ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_u(Bound))),
	    setarg(3,Att,strictness(Strict)),
	    reconsider_upper(CLP, X, Lin, Bound)
	;   % equal to upperbound: set new strictness
	    Strict is Sold \/ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
udus(t_l(L), CLP, X, Lin, Bound, Sold) :-
	compare_d(CLP, <, L, Bound),	% larger than lowerbound: update and reconsider basis
	Strict is Sold \/ 1,
	get_attr(X,clpcd_itf,Att),
	setarg(2,Att,type(t_lu(L,Bound))),
	setarg(3,Att,strictness(Strict)),
	reconsider_upper(CLP, X, Lin, Bound).
udus(t_lu(L,U), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, <, U, Bound)
	->  true	% larger than upperbound: keep
	;   compare_d(CLP, <, Bound, U)
	->  % smaller than upperbound: check lowerbound, possibly update and reconsider basis
	    compare_d(CLP, <, L, Bound),
	    Strict is Sold \/ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_lu(L,Bound))),
	    setarg(3,Att,strictness(Strict)),
	    reconsider_upper(CLP, X, Lin, Bound)
	;   % equal to upperbound: update strictness
	    Strict is Sold \/ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).

% uiu(Type,X,Lin,Bound,Strict)
%
% Updates upper bound of independent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new non-strict
% bound Bound.

uiu(t_none, _, X, _Lin, Bound, _) :-	% X had no bounds
	get_attr(X,clpcd_itf,Att),
	setarg(2,Att,type(t_u(Bound))),
	setarg(3,Att,strictness(0)).
uiu(t_u(U), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, <, U, Bound)
	->  true	% larger than upperbound: keep
	;   compare_d(CLP, <, Bound, U)
	->  % smaller than upperbound: update.
	    Strict is Sold /\ 2,	% update strictness: strictness of lowerbound is kept,
					% strictness of upperbound is set to non-strict
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_u(Bound))),
	    setarg(3,Att,strictness(Strict))
	;   true	% equal to upperbound and nonstrict: keep
	).
uiu(t_l(L), CLP, X, Lin, Bound, _Sold) :-
	(   compare_d(CLP, >, Bound, L)
	->   % Upperbound is larger than lowerbound: store new bound
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_lu(L,Bound)))
	;   compare_d(CLP, =, Bound, L),
            solve_bound(CLP, Lin, Bound) % Lowerbound was equal to new upperbound: solve
	).
uiu(t_L(L), CLP, X, Lin, Bound, _Sold) :-
	(   compare_d(CLP, >, Bound, L)
	->  % Same as for t_l (new bound becomes t_Lu)
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_Lu(L,Bound)))
	;   compare_d(CLP, =, Bound, L),
            solve_bound(CLP, Lin, Bound)	% Same as for t_l
	).
uiu(t_lu(L,U), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, <, Bound, U)
	->  (   compare_d(CLP, >, Bound, L)
	    ->	Strict is Sold /\ 2,
		get_attr(X,clpcd_itf,Att),
		setarg(2,Att,type(t_lu(L,Bound))),
		setarg(3,Att,strictness(Strict))
	    ;	% Lowerbound was equal to new bound: solve
                compare_d(CLP, =, Bound, L),
		Sold /\ 2 =:= 0,	% Only solve when strictness matches
		solve_bound(CLP, Lin, Bound)
	    )
	;   true	% Upperbound was equal to new bound and new bound non-strict: keep
	).
uiu(t_Lu(L,U), CLP, X, Lin, Bound, Sold) :-	% See t_lu case
	(   compare_d(CLP, <, Bound, U)
	->  (   compare_d(CLP, <, L, Bound)
	    ->  Strict is Sold /\ 2,
		get_attr(X,clpcd_itf,Att),
		setarg(2,Att,type(t_Lu(L,Bound))),
		setarg(3,Att,strictness(Strict))
	    ;   compare_d(CLP, =, L, Bound),
                Sold /\ 2 =:= 0,
		solve_bound(CLP, Lin, Bound)
	    )
	;   true
	).
uiu(t_U(U), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, <, Bound, U)
	->  Strict is Sold /\ 2,
	    (   get_attr(X,clpcd_itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		lb(ClassX, CLP, OrdX, Vlb-Vb-Lb),
		compare_d(CLP, =<, Bound, Lb + U)
	    ->  get_attr(X,clpcd_itf,Att2), % changed?
		setarg(2,Att2,type(t_U(Bound))),
		setarg(3,Att2,strictness(Strict)),
		pivot_a(CLP, Vlb, X, Vb, t_u(Bound)),
		reconsider(CLP, X)
	    ;   get_attr(X,clpcd_itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		setarg(2,Att,type(t_U(Bound))),
		setarg(3,Att,strictness(Strict)),
		Delta is Bound - U,
		backsubst_delta(CLP, ClassX, OrdX, X, Delta)
	    )
	;   true	% equal to upperbound and non-strict: keep
	).
uiu(t_lU(L,U), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, <, Bound, U)
	->  (   compare_d(CLP, <, L, Bound)
	    ->  % larger than lowerbound: see t_U case for rest
		Strict is Sold /\ 2,
		(   get_attr(X,clpcd_itf,Att),
		    arg(5,Att,order(OrdX)),
		    arg(6,Att,class(ClassX)),
		    lb(ClassX, CLP, OrdX, Vlb-Vb-Lb),
		    Bound - (Lb + U) < 1.0e-10
		->  get_attr(X,clpcd_itf,Att2), % changed?
		    setarg(2,Att2,type(t_lU(L,Bound))),
		    setarg(3,Att2,strictness(Strict)),
		    pivot_a(CLP, Vlb, X, Vb, t_lu(L,Bound)),
		    reconsider(CLP, X)
		;   get_attr(X,clpcd_itf,Att),
		    arg(5,Att,order(OrdX)),
		    arg(6,Att,class(ClassX)),
		    setarg(2,Att,type(t_lU(L,Bound))),
		    setarg(3,Att,strictness(Strict)),
		    Delta is Bound - U,
		    backsubst_delta(CLP, ClassX, OrdX, X, Delta)
		)
	    ;	% equal to lowerbound: check strictness and solve
                compare_d(CLP, =, L, Bound),
		Sold /\ 2 =:= 0,
		solve_bound(CLP, Lin, Bound)
	    )
	;   true	% equal to upperbound and non-strict: keep
			% smaller than upperbound: check lowerbound
	).

% uius(Type,X,Lin,Bound,Strict)
%
% Updates upper bound of independent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new strict
% bound Bound. (see also uiu/5)

uius(t_none, _, X, _Lin, Bound, _Sold) :-
	get_attr(X,clpcd_itf,Att),
	setarg(2,Att,type(t_u(Bound))),
	setarg(3,Att,strictness(1)).
uius(t_u(U), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, <, U, Bound)
	->  true
	;   compare_d(CLP, <, Bound, U)
	->  Strict is Sold \/ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_u(Bound))),
	    setarg(3,Att,strictness(Strict))
	;   Strict is Sold \/ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uius(t_l(L), CLP, X, _Lin, Bound, Sold) :-
	compare_d(CLP, <, L, Bound),
	Strict is Sold \/ 1,
	get_attr(X,clpcd_itf,Att),
	setarg(2,Att,type(t_lu(L,Bound))),
	setarg(3,Att,strictness(Strict)).
uius(t_L(L), CLP, X, _Lin, Bound, Sold) :-
        compare_d(CLP, <, L, Bound),
	Strict is Sold \/ 1,
	get_attr(X,clpcd_itf,Att),
	setarg(2,Att,type(t_Lu(L,Bound))),
	setarg(3,Att,strictness(Strict)).
uius(t_lu(L,U), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, <, U, Bound)
	->  true
	;   compare_d(CLP, <, Bound, U)
	->  compare_d(CLP, <, L, Bound),
	    Strict is Sold \/ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_lu(L,Bound))),
	    setarg(3,Att,strictness(Strict))
	;   Strict is Sold \/ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uius(t_Lu(L,U), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, <, U, Bound)
	->  true
	;   compare_d(CLP, <, Bound, U)
	->  compare_d(CLP, <, L, Bound),
	    Strict is Sold \/ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_Lu(L,Bound))),
	    setarg(3,Att,strictness(Strict))
	;   Strict is Sold \/ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uius(t_U(U), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, <, U, Bound)
	->  true
	;   compare_d(CLP, <, Bound, U)
	->  Strict is Sold \/ 1,
	    (   get_attr(X,clpcd_itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		lb(ClassX, CLP, OrdX, Vlb-Vb-Lb),
		Bound - (Lb + U) < 1.0e-10
	    ->  get_attr(X,clpcd_itf,Att2), % changed?
		setarg(2,Att2,type(t_U(Bound))),
		setarg(3,Att2,strictness(Strict)),
		pivot_a(CLP, Vlb, X, Vb, t_u(Bound)),
		reconsider(CLP, X)
	    ;   get_attr(X,clpcd_itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		setarg(2,Att,type(t_U(Bound))),
		setarg(3,Att,strictness(Strict)),
		Delta is Bound - U,
		backsubst_delta(CLP, ClassX, OrdX, X, Delta)
	    )
	;   Strict is Sold \/ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uius(t_lU(L,U), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, <, U, Bound)
	->  true
	;   compare_d(CLP, <, Bound, U)
	->  compare_d(CLP, <, L, Bound),
	    Strict is Sold \/ 1,
	    (   get_attr(X,clpcd_itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		lb(ClassX, CLP, OrdX, Vlb-Vb-Lb),
		Bound - (Lb + U) < 1.0e-10
	    ->  get_attr(X,clpcd_itf,Att2), % changed?
		setarg(2,Att2,type(t_lU(L,Bound))),
		setarg(3,Att2,strictness(Strict)),
		pivot_a(CLP, Vlb, X, Vb, t_lu(L,Bound)),
		reconsider(CLP, X)
	    ;	get_attr(X,clpcd_itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		setarg(2,Att,type(t_lU(L,Bound))),
		setarg(3,Att,strictness(Strict)),
		Delta is Bound - U,
		backsubst_delta(CLP, ClassX, OrdX, X, Delta)
	    )
	;   Strict is Sold \/ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).

% uil(Type,X,Lin,Bound,Strict)
%
% Updates lower bound of independent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new non-strict
% bound Bound. (see also uiu/5)


uil(t_none, _, X, _Lin, Bound, _Sold) :-
	get_attr(X,clpcd_itf,Att),
	setarg(2,Att,type(t_l(Bound))),
	setarg(3,Att,strictness(0)).
uil(t_l(L), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, >, Bound, L)
	->  Strict is Sold /\ 1,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_l(Bound))),
	    setarg(3,Att,strictness(Strict))
	;   true
	).
uil(t_u(U), CLP, X, Lin, Bound, _Sold) :-
	(   compare_d(CLP, <, Bound, U)
	->  get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_lu(Bound,U)))
	;   compare_d(CLP, =, Bound, U),
            solve_bound(CLP, Lin, Bound)
	).
uil(t_U(U), CLP, X, Lin, Bound, _Sold) :-
	(   compare_d(CLP, <, Bound, U)
	->  get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_lU(Bound,U)))
	;   compare_d(CLP, =, Bound, U),
            solve_bound(CLP, Lin, Bound)
	).
uil(t_lu(L,U), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, >, Bound, L)
	->  (   compare_d(CLP, <, Bound, U)
	    ->  Strict is Sold /\ 1,
		get_attr(X,clpcd_itf,Att),
		setarg(2,Att,type(t_lu(Bound,U))),
		setarg(3,Att,strictness(Strict))
	    ;   compare_d(CLP, =, Bound, U),
                Sold /\ 1 =:= 0,
		solve_bound(CLP, Lin, Bound)
	    )
	;   true
	).
uil(t_lU(L,U), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, >, Bound, L)
	->  (   compare_d(CLP, <, Bound, U)
	    ->  Strict is Sold /\ 1,
		get_attr(X,clpcd_itf,Att),
		setarg(2,Att,type(t_lU(Bound,U))),
		setarg(3,Att,strictness(Strict))
	    ;   compare_d(CLP, =, Bound, U),
                Sold /\ 1 =:= 0,
		solve_bound(CLP, Lin, Bound)
	    )
	;   true
	).
uil(t_L(L), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, >, Bound, L)
	->  Strict is Sold /\ 1,
	    (   get_attr(X,clpcd_itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		ub(ClassX, CLP, OrdX, Vub-Vb-Ub),
		compare_d(CLP, >=, Bound, Ub + L)
	    ->  get_attr(X,clpcd_itf,Att2), % changed?
		setarg(2,Att2,type(t_L(Bound))),
		setarg(3,Att2,strictness(Strict)),
		pivot_a(CLP, Vub, X, Vb, t_l(Bound)),
		reconsider(CLP, X)
	    ;   get_attr(X,clpcd_itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		setarg(2,Att,type(t_L(Bound))),
		setarg(3,Att,strictness(Strict)),
		Delta is Bound - L,
		backsubst_delta(CLP, ClassX, OrdX, X, Delta)
	    )
	;   true
	).
uil(t_Lu(L,U), CLP, X, Lin, Bound, Sold) :-
	(   compare_d(CLP, >, Bound, L)
	->  (   compare_d(CLP, <, Bound, U)
	    ->  Strict is Sold /\ 1,
		(   get_attr(X,clpcd_itf,Att),
		    arg(5,Att,order(OrdX)),
		    arg(6,Att,class(ClassX)),
		    ub(ClassX, CLP, OrdX, Vub-Vb-Ub),
		    compare_d(CLP, >=, Bound, Ub + L)
		->  get_attr(X,clpcd_itf,Att2), % changed?
		    setarg(2,Att2,t_Lu(Bound,U)),
		    setarg(3,Att2,strictness(Strict)),
		    pivot_a(CLP, Vub, X, Vb, t_lu(Bound,U)),
		    reconsider(CLP, X)
		;   get_attr(X,clpcd_itf,Att),
		    arg(5,Att,order(OrdX)),
		    arg(6,Att,class(ClassX)),
		    setarg(2,Att,type(t_Lu(Bound,U))),
		    setarg(3,Att,strictness(Strict)),
		    Delta is Bound - L,
		    backsubst_delta(CLP, ClassX, OrdX, X, Delta)
		)
	    ;	compare_d(CLP, =, Bound, U),
                Sold /\ 1 =:= 0,
		solve_bound(CLP, Lin, Bound)
	    )
	;   true
	).

% uils(Type,X,Lin,Bound,Strict)
%
% Updates lower bound of independent variable X with linear equation
% Lin that had type Type and strictness Strict, to the new strict
% bound Bound. (see also uiu/5)

uils(t_none, _, X, _Lin, Bound, _Sold) :-
	get_attr(X,clpcd_itf,Att),
	setarg(2,Att,type(t_l(Bound))),
	setarg(3,Att,strictness(2)).
uils(t_l(L), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, <, Bound, L)
	->  true
	;   compare_d(CLP, >, Bound, L)
	->  Strict is Sold \/ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_l(Bound))),
	    setarg(3,Att,strictness(Strict))
	;   Strict is Sold \/ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uils(t_u(U), CLP, X, _Lin, Bound, Sold) :-
        compare_d(CLP, <, Bound, U),
	Strict is Sold \/ 2,
	get_attr(X,clpcd_itf,Att),
	setarg(2,Att,type(t_lu(Bound,U))),
	setarg(3,Att,strictness(Strict)).
uils(t_U(U), CLP, X, _Lin, Bound, Sold) :-
        compare_d(CLP, <, Bound, U),
	Strict is Sold \/ 2,
	get_attr(X,clpcd_itf,Att),
	setarg(2,Att,type(t_lU(Bound,U))),
	setarg(3,Att,strictness(Strict)).
uils(t_lu(L,U), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, <, Bound, L)
	->  true
	;   compare_d(CLP, >, Bound, L)
	->  compare_d(CLP, <, Bound, U),
	    Strict is Sold \/ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_lu(Bound,U))),
	    setarg(3,Att,strictness(Strict))
	;   Strict is Sold \/ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uils(t_lU(L,U), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, <, Bound, L)
	->  true
	;   compare_d(CLP, >, Bound, L)
	->  compare_d(CLP, <, Bound, U),
	    Strict is Sold \/ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(2,Att,type(t_lU(Bound,U))),
	    setarg(3,Att,strictness(Strict))
	;   Strict is Sold \/ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uils(t_L(L), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, <, Bound, L)
	->  true
	;   compare_d(CLP, >, Bound, L)
	->  Strict is Sold \/ 2,
	    (   get_attr(X,clpcd_itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		ub(ClassX, CLP, OrdX, Vub-Vb-Ub),
		compare_d(CLP, >=, Bound, Ub + L)
	    ->  get_attr(X,clpcd_itf,Att2), % changed?
		setarg(2,Att2,type(t_L(Bound))),
		setarg(3,Att2,strictness(Strict)),
		pivot_a(CLP, Vub, X, Vb, t_l(Bound)),
		reconsider(CLP, X)
	    ;   get_attr(X,clpcd_itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		setarg(2,Att,type(t_L(Bound))),
		setarg(3,Att,strictness(Strict)),
		Delta is Bound - L,
		backsubst_delta(CLP, ClassX, OrdX, X, Delta)
	    )
	;   Strict is Sold \/ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).
uils(t_Lu(L,U), CLP, X, _Lin, Bound, Sold) :-
	(   compare_d(CLP, <, Bound, L)
	->  true
	;   compare_d(CLP, >, Bound, L)
	->  compare_d(CLP, <, Bound, U),
	    Strict is Sold \/ 2,
	    (   get_attr(X,clpcd_itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		ub(ClassX, CLP, OrdX, Vub-Vb-Ub),
		compare_d(CLP, >=, Bound, Ub + L)
	    ->  get_attr(X,clpcd_itf,Att2), % changed?
		setarg(2,Att2,type(t_Lu(Bound,U))),
		setarg(3,Att2,strictness(Strict)),
		pivot_a(CLP, Vub, X, Vb, t_lu(Bound,U)),
		reconsider(CLP, X)
	    ;   get_attr(X,clpcd_itf,Att),
		arg(5,Att,order(OrdX)),
		arg(6,Att,class(ClassX)),
		setarg(2,Att,type(t_Lu(Bound,U))),
		setarg(3,Att,strictness(Strict)),
		Delta is Bound - L,
		backsubst_delta(CLP, ClassX, OrdX, X, Delta)
	    )
	;   Strict is Sold \/ 2,
	    get_attr(X,clpcd_itf,Att),
	    setarg(3,Att,strictness(Strict))
	).

% reconsider_upper(X,Lin,U)
%
% Checks if the upperbound of X which is U, satisfies the bounds
% of the variables in Lin: let R be the sum of all the bounds on
% the variables in Lin, and I be the inhomogene part of Lin, then
% upperbound U should be larger or equal to R + I (R may contain
% lowerbounds).
% See also rcb/3 in bv.pl

reconsider_upper(CLP, X, [I,R|H], U) :-
	compare_d(CLP, >=, R + I, U),	% violation
	!,
	dec_step(H, CLP, Status),	% we want to decrement R
	rcbl_status(Status, CLP, X, [], Binds, [], u(U)),
	export_binding(Binds).
reconsider_upper(_, _, _, _).

% reconsider_lower(X,Lin,L)
%
% Checks if the lowerbound of X which is L, satisfies the bounds
% of the variables in Lin: let R be the sum of all the bounds on
% the variables in Lin, and I be the inhomogene part of Lin, then
% lowerbound L should be smaller or equal to R + I (R may contain
% upperbounds).
% See also rcb/3 in bv.pl

reconsider_lower(CLP, X, [I,R|H], L) :-
	compare_d(CLP, =<, R + I, L),	% violation
	!,
	inc_step(H, CLP, Status),	% we want to increment R
	rcbl_status(Status, CLP, X, [], Binds, [], l(L)),
	export_binding(Binds).
reconsider_lower(_, _, _, _).

%
% lin is dereferenced
%

% solve_bound(Lin,Bound)
%
% Solves the linear equation Lin - Bound = 0
% Lin is the linear equation of X, a variable whose bounds have narrowed to value Bound

solve_bound(CLP, Lin, Bound) :-
	compare_d(CLP, =, Bound, 0),
	!,
	solve(CLP, Lin).
solve_bound(CLP, Lin, Bound) :-
	Nb is -Bound,
	normalize_scalar(Nb,Nbs),
	add_linear_11(CLP, Nbs, Lin, Eq),
	solve(CLP, Eq).
