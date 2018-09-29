:- module(float_props,
          [evaluable/1,
           domain_substitution/2]).

:- use_module(library(assertions)).
:- use_module(library(extend_args)).

:- global evaluable/1.

evaluable(G) :- call(G).

domain_substitution(complexn(A), A=complexn(_, _)) :- !.
domain_substitution(floatn(A), A=floatn(_)) :- !.
domain_substitution(Prop1, A=Prop) :-
    extend_args(Prop, [A], Prop1).
