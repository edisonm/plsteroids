:- module(cdqr,
          [num_arithmetic_function/1]).

:- use_module(library(arithmetic)).
:- use_module(library(mapnargs)).
:- use_module(library(lists)).
:- use_module(library(neck)).
:- use_module(library(clpcd/domain_ops)).
:- use_module(library(clpcd/nf)).

% crafted to get float operators

:- public curr_num_arithmetic_function/1.

:- meta_predicate curr_num_arithmetic_function(?).

setnum(acosh(_), N, X) :- !, arithmetic_expression_value(1.1*N, X).
setnum(_, N, X) :- arithmetic_expression_value(0.8/N, X).

curr_num_arithmetic_function(Expr) :-
    current_arithmetic_function(Expr),
    \+ \+ ( mapnargs(setnum(Expr), Expr),
            catch(arithmetic_expression_value(Expr, Value),
                  _,
                  fail),
            float(Value)
           ).

num_arithmetic_function(Expr) :-
    curr_num_arithmetic_function(Expr),
    neck.

cdqr_nl_eval(F, R) :-
    num_arithmetic_function(F),
    neck,
    R is F.

:- public curr_cdqr_nonlin_af/4.

curr_cdqr_nonlin_af(Term, AL, Skel, SL) :-
    num_arithmetic_function(Term),
    \+ member(Term, [_^_, _**_, +_, -_, _-_, _+_, _/_, _*_,
                     pow(_, _), float(_), eval(_)]),
    functor(Term, F, A),
    functor(Skel, F, A),
    A > 0,
    Term =.. [_|AL],
    Skel =.. [_|SL].

cdqr_nonlin(pow(A, B), [A, B], X^Y, [X, Y]).
cdqr_nonlin(A^B,       [A, B], X^Y, [X, Y]).
cdqr_nonlin(A**B,      [A, B], X^Y, [X, Y]).
cdqr_nonlin(Term, AL, Skel, SL) :-
    curr_cdqr_nonlin_af(Term, AL, Skel, SL),
    neck.

cdqr_nl_invertible(sin(_)).
cdqr_nl_invertible(abs(_)).
cdqr_nl_invertible(cos(_)).
cdqr_nl_invertible(tan(_)).
cdqr_nl_invertible(asin(_)).
cdqr_nl_invertible(acos(_)).
cdqr_nl_invertible(atan(_)).
cdqr_nl_invertible(sinh(_)).
cdqr_nl_invertible(cosh(_)).
cdqr_nl_invertible(tanh(_)).
cdqr_nl_invertible(asinh(_)).
cdqr_nl_invertible(acosh(_)).
cdqr_nl_invertible(atanh(_)).
cdqr_nl_invertible(exp(_)).
cdqr_nl_invertible(log(_)).
cdqr_nl_invertible(log10(_)).
cdqr_nl_invertible(sqrt(_)).
cdqr_nl_invertible(_^_).

cdqr_nl_invert(sin(X),_,X,Y,Res) :- abs(Y) =< 1, int(I), Res is asin(Y)+2*pi*I.
cdqr_nl_invert(cos(X),_,X,Y,Res) :- abs(Y) =< 1, int(I), Res is acos(Y)+2*pi*I.
cdqr_nl_invert(tan(X),_,X,Y,Res) :- int(I), Res is atan(Y)+pi*I.
cdqr_nl_invert(asin(X),_,X,Y,Res) :- Res is sin(Y).
cdqr_nl_invert(acos(X),_,X,Y,Res) :- Res is cos(Y).
cdqr_nl_invert(atan(X),_,X,Y,Res) :- Res is tan(Y).
cdqr_nl_invert(sinh(X),_,X,Y,Res) :- Res is asinh(Y).
cdqr_nl_invert(cosh(X),_,X,Y,Res) :- Y >= 1, Res is acosh(Y).
cdqr_nl_invert(tanh(X),_,X,Y,Res) :- Res is atanh(Y).
cdqr_nl_invert(asinh(X),_,X,Y,Res) :- Res is sinh(Y).
cdqr_nl_invert(acosh(X),_,X,Y,Res) :- Res is cosh(Y).
cdqr_nl_invert(atanh(X),_,X,Y,Res) :- Res is tanh(Y).
cdqr_nl_invert(log(X),_,X,Y,Res) :- Res is exp(Y).
cdqr_nl_invert(log10(X),_,X,Y,Res) :- Res is 10^Y.
cdqr_nl_invert(sqrt(X),_,X,Y,Res) :- Res is Y^2.
cdqr_nl_invert(exp(X),_,X,Y,Res) :- Y >= 0, Res is log(Y).
cdqr_nl_invert(abs(X),_,X,Y,Res) :-
    ( Y > 0
    ->( Res is Y
      ; Res is -Y
      )
    ; Y =:= 0
    ->Res is Y
    ).
cdqr_nl_invert(B^C,CLP,X,A,Res) :-
	(   nf_constant(B,Kb)
	->  A > 0,
	    Kb > 0,
            % Kb =\= 1.0
            compare_d(CLP, \=, Kb, 1),
	    X = C, % note delayed unification
	    Res is log(A)/log(Kb)
	;   nf_constant(C,Kc),
            A =\= 0,
	    Kc > 0,
	    X = B, % note delayed unification
	    Res is A**(1/Kc)
	).

:- public cdqr/1.

cdqr(cdq).
cdqr(cdr).

clpcd_nf:nl_eval(C, F, R) :-
        cdqr(C),
        neck,
        cdqr_nl_eval(F, R).

clpcd_nf:nl_invertible(C,F) :-
        cdqr(C),
        neck,
        cdqr_nl_invertible(F).

clpcd_nf:nl_invert(C,F,X,Y,Res) :-
        cdqr(C),
        neck,
        cdqr_nl_invert(F,C,X,Y,N),
        cast_d(C,N,Res).

clpcd_nf:nonlin(C, Term, AL, Skel, SL) :-
        cdqr(C),
        neck,
        cdqr_nonlin(Term, AL, Skel, SL).

