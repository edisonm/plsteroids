:- module(test_impl1, []).

:- use_module(library(interface)).
:- init_expansors.

:- implements(test_intf).

intf_pred(A, B, C) :-
    C is A rdiv B.

intf_meta(A, A).
