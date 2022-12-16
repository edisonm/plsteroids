:- begin_tests(cohesive).

:- use_module(c4).
:- use_module(c2).

test(c4) :-
    findall(X, q(X), L),
    assertion(L = [d, a, b, c, d, x, y]).

test(c2) :-
    findall(X, c2:p1(X), L),
    assertion(L = [a, b, c, d, x, y]).

test(c5) :-
    findall(X, c5:p1(X), L),
    assertion(L = [x, y]).

:- end_tests(cohesive).
