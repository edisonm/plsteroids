/*  Constraint logic programming over continuous domains

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2020, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(clpcd_inv,
          [cd_invertible/1,
           cd_invert/5,
           cd_nonlin/4,
           num_arithmetic_function/1]).

:- use_module(library(arithmetic)).
:- use_module(library(lists)).
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

cd_invert(sin(  X),T,X,Y,R) :- compare_d(T, =<, abs(Y), 1), int(I), eval_d(T, asin(Y)+2*pi*I, R).
cd_invert(cos(  X),T,X,Y,R) :- compare_d(T, =<, abs(Y), 1), int(I), eval_d(T, acos(Y)+2*pi*I, R).
cd_invert(tan(  X),T,X,Y,R) :- int(I), eval_d(T,atan(Y)+pi*I,R).
cd_invert(asin( X),T,X,Y,R) :- eval_d(T, sin(  Y), R).
cd_invert(acos( X),T,X,Y,R) :- eval_d(T, cos(  Y), R).
cd_invert(atan( X),T,X,Y,R) :- eval_d(T, tan(  Y), R).
cd_invert(sinh( X),T,X,Y,R) :- eval_d(T, asinh(Y), R).
cd_invert(cosh( X),T,X,Y,R) :- compare_d(T, >=, Y, 1), eval_d(T, acosh(Y), R).
cd_invert(tanh( X),T,X,Y,R) :- eval_d(T, atanh(Y), R).
cd_invert(asinh(X),T,X,Y,R) :- eval_d(T, sinh( Y), R).
cd_invert(acosh(X),T,X,Y,R) :- eval_d(T, cosh( Y), R).
cd_invert(atanh(X),T,X,Y,R) :- eval_d(T, tanh( Y), R).
cd_invert(log(  X),T,X,Y,R) :- eval_d(T, exp(  Y), R).
cd_invert(log10(X),T,X,Y,R) :- eval_d(T, exp10(Y), R).
cd_invert(log1p(X),T,X,Y,R) :- eval_d(T, expm1(Y), R).
cd_invert(log2( X),T,X,Y,R) :- eval_d(T, exp2( Y), R).
cd_invert(sqrt( X),T,X,Y,R) :- eval_d(T, Y^2, R).
cd_invert(cbrt( X),T,X,Y,R) :- eval_d(T, Y^3, R).
cd_invert(exp10(X),T,X,Y,R) :- compare_d(T, >=, Y,  0), eval_d(T, log10(Y), R).
cd_invert(exp2( X),T,X,Y,R) :- compare_d(T, >=, Y,  0), eval_d(T, log2( Y), R).
cd_invert(exp(  X),T,X,Y,R) :- compare_d(T, >=, Y,  0), eval_d(T, log(  Y), R).
cd_invert(expm1(X),T,X,Y,R) :- compare_d(T, >=, Y, -1), eval_d(T, log1p(Y), R).
cd_invert(abs(  X),T,X,Y,R) :-
    ( compare_d(T, <, 0, Y)
    ->( R = Y
      ; eval_d(T, -Y, R)
      )
    ; compare_d(T, =, 0, Y)
    ->R = Y
    ).
cd_invert(B^C,T,X,A,R) :-
    ( nf_constant(B, Kb)
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
cd_invert(hypot(B,C),T,X,A,R) :-
    ( nf_constant(B, Kb)
    ->cd_invert_hypot(Kb,C,T,X,A,R)
    ; nf_constant(C, Kc)
    ->cd_invert_hypot(Kc,B,T,X,A,R)
    ).

cd_invert_hypot(Kb,C,T,X,A,R) :-
    compare_d(T, >=, abs(A), abs(Kb)),
    X = C,
    eval_d(T, sqrt((A+Kb)*(A-Kb)), R).

cd_invertible(Term) :-
    clause(cd_invert(Term, _, _, _, _), _),
    neck.

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
% Extra arithmetic functions not implemented in is/2 but in most of float libraries
curr_num_arithmetic_function(Expr) :-
    member(Expr, [cbrt(_), exp10(_), exp2(_), expm1(_), log1p(_),
                  log2(_), tgamma(_), hypot(_, _)]),
    neck.

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
    A >= 0,
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
