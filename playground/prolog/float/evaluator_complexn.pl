:- module(evaluator_complexn, []).

:- use_module(library(extend_args)).
:- use_module(library(float/evaluator)).
:- use_module(library(float/complexn)).

evaluator:castexpr(    complexn(P, E), complexn(P), E) :- integer(P).
evaluator:evalfunc(    complexn(P), E, V) :- complexn_evalfunc(E, P, P, V).
evaluator:domain_value(complexn(P), E, V) :- complexn_new_value(E, P, P, V).
evaluator:domain_order(complexn(P), [5, P]).
evaluator:domain_args( complexn(_), FD) :- complexn_domain_args(FD).

evaluator:castexpr(    complexn(R, I, E), complexn(R, I), E) :- integer(R), integer(I).
evaluator:evalfunc(    complexn(R, I), E, V) :- complexn_evalfunc(E, R, I, V).
evaluator:domain_value(complexn(R, I), E, V) :- complexn_new_value(E, R, I, V).
evaluator:domain_order(complexn(R, I), [5, N]) :- N is max(R, I).
evaluator:domain_args( complexn(_, _), FD) :- complexn_domain_args(FD).
