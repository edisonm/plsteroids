:- module(termhide_example, [te/1]).

:- reexport(library(termhide)).
:- init_expansors.

:- hide mykey/1.

te(A) :-
    A = mykey(x).
