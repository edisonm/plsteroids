:- module(clpcd,
          [cd_invertible/1,
           cd_invert/5,
           cd_nonlin/4,
           num_arithmetic_function/1]).

:- use_module(library(neck)).
:- use_module(library(mapnargs)).
:- use_module(library(typeprops)).
:- use_module(library(clpcd/domain_ops)).
:- use_module(library(clpcd/nf)).
:- use_module(library(clpcd/dump)).

		 /*******************************
		 *	 TOPLEVEL PRINTING	*
		 *******************************/

:- multifile
	prolog:message/3,
        prolog_colour:syntax_message//1.

prolog_colour:syntax_message(constraint(clpcd(Sub))) -->
	[ 'clp(~w) constraint'-[Sub] ].
prolog_colour:syntax_message(type_error(constraint(clpcd(Sub)))) -->
	[ 'Only clp(~w) constraints may appear inside {}'-[Sub] ].

prolog:message(query(YesNo,Bindings)) --> !,
	{dump_toplevel_bindings(Bindings,Constraints)},
	dump_format(Constraints),
        '$messages':prolog_message(query(YesNo,Bindings)).

cd_invertible(sin(_)).
cd_invertible(abs(_)).
cd_invertible(cos(_)).
cd_invertible(tan(_)).
cd_invertible(asin(_)).
cd_invertible(acos(_)).
cd_invertible(atan(_)).
cd_invertible(sinh(_)).
cd_invertible(cosh(_)).
cd_invertible(tanh(_)).
cd_invertible(asinh(_)).
cd_invertible(acosh(_)).
cd_invertible(atanh(_)).
cd_invertible(exp(_)).
cd_invertible(log(_)).
cd_invertible(log10(_)).
cd_invertible(sqrt(_)).
cd_invertible(_^_).

cd_invert(sin(X),T,X,Y,R) :- compare_d(T, =<, abs(Y), 1), int(I), eval_d(T, asin(Y) + 2*pi * I, R).
cd_invert(cos(X),T,X,Y,R) :- compare_d(T, =<, abs(Y), 1), int(I), eval_d(T, acos(Y)+2*pi*I, R).
cd_invert(tan(X),T,X,Y,R) :- int(I), eval_d(T,atan(Y)+pi*I,R).
cd_invert(asin(X),T,X,Y,R) :- eval_d(T, sin(Y), R).
cd_invert(acos(X),T,X,Y,R) :- eval_d(T, cos(Y), R).
cd_invert(atan(X),T,X,Y,R) :- eval_d(T, tan(Y), R).
cd_invert(sinh(X),T,X,Y,R) :- eval_d(T, asinh(Y), R).
cd_invert(cosh(X),T,X,Y,R) :- compare_d(T, >=, Y, 1), eval_d(T, acosh(Y), R).
cd_invert(tanh(X),T,X,Y,R) :- eval_d(T, atanh(Y), R).
cd_invert(asinh(X),T,X,Y,R) :- eval_d(T, sinh(Y), R).
cd_invert(acosh(X),T,X,Y,R) :- eval_d(T, cosh(Y), R).
cd_invert(atanh(X),T,X,Y,R) :- eval_d(T, tanh(Y), R).
cd_invert(log(X),T,X,Y,R) :- eval_d(T, exp(Y), R).
cd_invert(log10(X),T,X,Y,R) :- eval_d(T, 10^Y, R).
cd_invert(sqrt(X),T,X,Y,R) :- eval_d(T, Y^2, R).
cd_invert(exp(X),T,X,Y,R) :- compare_d(T, >=, Y, 0), eval_d(T, log(Y), R).
cd_invert(abs(X),T,X,Y,R) :-
    ( compare_d(T, >, Y, 0 )
    ->( eval_d(T, Y, R)
      ; eval_d(T, -Y, R)
      )
    ; compare_d(T, =, Y, 0 )
    ->eval_d(T, Y, R)
    ).
cd_invert(B^C,T,X,A,R) :-
    ( nf_constant(B,Kb)
    ->compare_d(T, <, 0, A),
      compare_d(T, <, 0, Kb),
      % Kb =\= 1.0
      compare_d(T, \=, Kb, 1),
      X = C, % note delayed unification
      eval_d(T, log(A)/log(Kb), R)
    ; nf_constant(C,Kc),
      compare_d(T, \=, 0, A),
      compare_d(T, <, 0, Kc),
      X = B, % note delayed unification
      eval_d(T, A**(1/Kc), R)
    ).

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

:- public curr_cd_nonlin_af/4.

curr_cd_nonlin_af(Term, AL, Skel, SL) :-
    num_arithmetic_function(Term),
    \+ member(Term, [_^_, _**_, +_, -_, _-_, _+_, _/_, _*_,
                     pow(_, _), float(_), eval(_)]),
    functor(Term, F, A),
    functor(Skel, F, A),
    A > 0,
    Term =.. [_|AL],
    Skel =.. [_|SL].

cd_nonlin(pow(A, B), [A, B], X^Y, [X, Y]).
cd_nonlin(A^B,       [A, B], X^Y, [X, Y]).
cd_nonlin(A**B,      [A, B], X^Y, [X, Y]).
cd_nonlin(Term, AL, Skel, SL) :-
    curr_cd_nonlin_af(Term, AL, Skel, SL),
    neck.

dump_toplevel_bindings(Bindings,Constraints) :-
	dump_vars_names(Bindings,[],Vars,Names),
	dump(Vars,Names,Constraints).

dump_vars_names([],_,[],[]).
dump_vars_names([Name=Term|Rest],Seen,Vars,Names) :-
	(   var(Term),
	    (   get_attr(Term,clpcd_itf,_)
	    ;   get_attr(Term,clpcd_geler,_)
	    ),
	    \+ memberchk_eq(Term,Seen)
	->  Vars = [Term|RVars],
	    Names = [Name|RNames],
	    NSeen = [Term|Seen]
	;   Vars = RVars,
	    Names = RNames,
	    Seen = NSeen
	),
	dump_vars_names(Rest,NSeen,RVars,RNames).

dump_format([]) --> [].
dump_format([X|Xs]) -->
	['{~w}'-[X], nl],
	dump_format(Xs).

memberchk_eq(X,[Y|Ys]) :-
	(   X == Y
	->  true
	;   memberchk_eq(X,Ys)
	).
