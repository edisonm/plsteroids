:- module(p1,
          [ e1/2,
            e2/2,
            e3/2
          ]).

:- use_module(library(neck)).
:- use_module(library(lists)).
:- init_expansors.

q1(1, a).
q1(2, b).
q1(3, c).

a1(H) :-
    functor(H, f, 2),
    arg(1, H, g(A)),
    arg(2, H, h(A)),
    A is sqrt(2),
    neck.

a2(H) :-
    member(X, [1,2,3]),
    neck,
    a3(X, H).

a3(_, _).

e1(A, F) :-
    q1(A, B),
    upcase_atom(B, U),
    neck,
    p2(U, C),
    p3(C, D),
    p2(D, E),
    p3(E, F).
e1(A, F) :-
    q1(A, B),
    neck,
    !,
    p2(B, C),
    p3(C, D),
    p2(D, E),
    p3(E, F).
e1(A, F) :-
    q1(A, B),
    neck,
    p2(B, C),
    p3(C, D),
    !,
    p2(D, E),
    p3(E, F).
e1(A, D) :-
    q1(B, A),
    neck,
    p2(B, C),
    p3(C, D).

e2(A, C) :-
    q1(A, B),
    neck,
    p2(B, C).

e3(A, B) :-
    q1(A, B),
    neck.

% Next clause must trigger a performance warning, but not an error:
e4(A, A) :-
    neck.

p2(A, A).

p3(A, A).
