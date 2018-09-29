:- module(evaluator_num, []).

:- use_module(library(arithmetic)).
:- use_module(library(neck)).
:- use_module(library(mapnargs)).
:- use_module(library(float/evaluator)).

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

evaluator:castexpr(num(Expr), num, Expr).
evaluator:evalfunc(num, Expr, Value) :- arithmetic_expression_value(Expr, Value).
evaluator:domain_value(num, Value, Value) :- number(Value).
evaluator:domain_order(num, [3]).
evaluator:domain_args(num, Expr) :-
    num_arithmetic_function(Expr),
    term_variables(Expr, Vars),
    maplist(=(num), Vars).
