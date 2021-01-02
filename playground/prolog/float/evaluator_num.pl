:- module(evaluator_num, []).

:- use_module(library(arithmetic)).
:- use_module(library(float/evaluator)).
:- use_module(library(clpcd/inv)).

evaluator:castexpr(num(Expr), num, Expr).
evaluator:evalfunc(num, Expr, Value) :- arithmetic_expression_value(Expr, Value).
evaluator:domain_value(num, Value, Value) :- number(Value).
evaluator:domain_order(num, [3]).
evaluator:domain_args(num, Expr) :-
    num_arithmetic_function(Expr),
    term_variables(Expr, Vars),
    maplist(=(num), Vars).
