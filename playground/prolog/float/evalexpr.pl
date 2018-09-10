:- module(evalexpr,
          [evalexpr/3]).

:- use_module(library(list_sequence)).
:- use_module(library(neck)).
:- use_module(library(lists)).

:- multifile
        evaluable/2,
        new_value/3,
        eval_func/3,
        eval_hook/3.

:- discontiguous
        evaluable/2,
        new_value/3,
        eval_func/3,
        eval_hook/3.

add_evalexpr(D, A, V, evalexpr(D, A, V)).

evalexpr(Domain, Var, _) :-
    ( var(Domain)
    ; var(Var)
    ),
    !,
    throw(error(instantiation_error, context(evalexpr:evalexpr/3, _))).
evalexpr(Domain, Expr, Value) :-
    new_value(Domain, Expr, Value),
    !.
evalexpr(Domain, Expr, Value) :-
    evaluable(Domain, Expr),
    Expr =.. [F|AL],
    maplist(evalexpr(Domain), AL, VL),
    EV =.. [F|VL],
    !,
    eval_func(Domain, EV, Value).
evalexpr(Domain, Expr, Value) :-
    eval_hook(Domain, Expr, Value),
    !.
evalexpr(Domain, Expr, Value) :-
    evalarith(Expr, Domain, Value).

evalarith(Expr, Domain, Value) :-
    current_arithmetic_function(Expr),
    Expr =.. [F|AL],
    maplist(add_evalexpr(Domain), AL, VL, EL),
    EV =.. [F|VL],
    append(EL, [eval_func(Domain, EV, Value)], CL),
    list_sequence(CL, CV),
    neck,
    CV.
