:- module(termhide_example_2, [te2/1]).

:- use_module(termhide_example).
:- init_expansors.

te2(X) :-
    te(X).
te2(mykey(1)).
te2(Y) :-
    eq(Y, mykey(2)).

eq(X, X).
