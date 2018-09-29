:- module(evaluator_int, []).

:- use_module(library(arithmetic)).
:- use_module(library(neck)).
:- use_module(library(mapnargs)).
:- use_module(library(float/evaluator)).

% Note that the way cast occurs in is/2 is not compatible with having a
% multi-domain arithmetic evaluator: you would like to have a domain where you
% can not escape to a super-domain, for instance, the arithmetic int/int would
% be evaluated to a float, which is not good.

% crafted to get integer operators
setint(N, N).


int_arithmetic_function(Expr) :-
    current_arithmetic_function(Expr),
    \+ \+ ( mapnargs(setint, Expr),
            catch(arithmetic_expression_value(Expr, Value),
                  _,
                  fail),
            integer(Value)
           ),
    neck.

evaluator:castexpr(int(Expr), int, Expr).
evaluator:evalfunc(int, Expr, Value) :- arithmetic_expression_value(Expr, Value).
evaluator:domain_value(int, Value, Value) :- integer(Value).
evaluator:domain_order(int, [1]).
evaluator:domain_args(int, Expr) :-
    int_arithmetic_function(Expr),
    term_variables(Expr, Vars),
    maplist(=(int), Vars).
