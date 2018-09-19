:- module(evaluator_floatn, []).

:- use_module(library(extend_args)).
:- use_module(library(float/evaluator)).
:- use_module(library(float/floatn)).

evaluator:castexpr(    floatn(P, E), floatn(P), E) :- integer(P).
evaluator:evalfunc(    floatn(P), E, V) :- floatn_evalfunc(E, P, V).
evaluator:domain_value(floatn(P), E, V) :- floatn_new_value(E, P, V).
evaluator:domain_order(floatn(P), [4, P]).
evaluator:domain_args( floatn(_), FD) :- floatn_domain_args(FD).
