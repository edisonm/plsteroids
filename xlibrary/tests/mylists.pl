:- module(mylists, [myappend/3]).

myappend(A,B,C) :- writeln(myappend(A,B,C)),fail.
myappend([X|Y], Z, [X|T]) :-
    myappend(Y, Z, T).
myappend([], T, T).
