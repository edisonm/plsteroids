:- module(evaluator_rat, []).

:- use_module(library(arithmetic)).
:- use_module(library(neck)).
:- use_module(library(mapnargs)).
:- use_module(library(float/evaluator)).

% crafted to get float operators

:- public curr_rat_arithmetic_function/1.

:- meta_predicate curr_rat_arithmetic_function(?).

setnum(_, N, X) :- arithmetic_expression_value((4 rdiv 5)/N, X).

curr_rat_arithmetic_function(Expr) :-
    current_arithmetic_function(Expr),
    \+ \+ ( mapnargs(setnum(Expr), Expr),
            catch(arithmetic_expression_value(Expr, Value),
                  _,
                  fail),
            rational(Value)
           ).

rat_arithmetic_function(Expr) :-
    curr_rat_arithmetic_function(Expr),
    neck.

evaluator:castexpr(rat(Expr), rat, Expr).
evaluator:evalfunc(rat, Expr, Value) :- arithmetic_expression_value(Expr, Value).
evaluator:domain_value(rat, Value, Value) :- rational(Value).
evaluator:domain_order(rat, [2]).
evaluator:domain_args(rat, Expr) :-
    rat_arithmetic_function(Expr),
    term_variables(Expr, Vars),
    maplist(=(rat), Vars).
