:- module(termhide_example_3, [te3/1]).

:- use_module(termhide_example_2).
:- init_expansors.

te3(Y) :-
    Y = mykey(2).
te3(Y) :-
    te2(Y).
