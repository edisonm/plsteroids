:- module(detact,
          [determine_active_dec/1,
           determine_active_inc/1]).

:- use_module(library(clpcd/solve)).

% determine_active_dec(Lin)
%
% Activates inactive bounds on the variables of Lin if such bounds exist.
% If the type of a variable is t_none, this fails. This version is aimed
% to make the R component of Lin as small as possible in order not to violate
% an upperbound (see reconsider/2)

determine_active_dec([_,_|H]) :-
	determine_active(H,-1).

% determine_active_inc(Lin)
%
% Activates inactive bounds on the variables of Lin if such bounds exist.
% If the type of a variable is t_none, this fails. This version is aimed
% to make the R component of Lin as large as possible in order not to violate
% a lowerbound (see reconsider/2)

determine_active_inc([_,_|H]) :-
	determine_active(H,1).

% determine_active(Hom,S)
%
% For each variable in Hom, activates its bound if it is not yet activated.
% For the case of t_lu(_,_) the lower or upper bound is activated depending on
% K and S:
% If sign of K*S is negative, then lowerbound, otherwise upperbound.

determine_active([],_).
determine_active([l(X*K,_)|Xs],S) :-
	get_attr(X,clpcd_itf,Att),
	arg(2,Att,type(Type)),
	determine_active(Type,X,K,S),
	determine_active(Xs,S).

determine_active(t_L(_),_,_,_).
determine_active(t_Lu(_,_),_,_,_).
determine_active(t_U(_),_,_,_).
determine_active(t_lU(_,_),_,_,_).
determine_active(t_l(L),X,_,_) :- intro_at(X,L,t_L(L)).
determine_active(t_u(U),X,_,_) :- intro_at(X,U,t_U(U)).
determine_active(t_lu(L,U),X,K,S) :-
	KS is K*S,
	(   KS < 0
	->  intro_at(X,L,t_Lu(L,U))
	;   KS > 0
	->  intro_at(X,U,t_lU(L,U))
	).
