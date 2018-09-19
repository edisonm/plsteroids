:- module(evalexpr_floatn, []).

:- use_module(library(float/evalexpr)).

evalexpr:evaluable(floatn, E) :- evalexpr:evaluable(floatn(_), E).
evalexpr:new_value(floatn, E, V) :- evalexpr:new_value(floatn(_), E, V).
evalexpr:eval_func(floatn, F, V) :- evalexpr:eval_func(floatn(_), F, V).
evalexpr:eval_hook(floatn, E, V) :- evalexpr:eval_hook(floatn(_), E, V).

evalexpr:evaluable(floatn(_), E) :- evalexpr_evaluable(E).
evalexpr:new_value(floatn(P), E, V) :- floatn_new_value(E, P, V).
evalexpr:eval_func(floatn(P), F, V) :- once(evalfunc_floatn(F, P, V)).
evalexpr:eval_hook(floatn(P), E, V) :- evalexpr_floatn(E, P, V).
