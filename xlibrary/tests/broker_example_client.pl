:- module(broker_example_client, []).

:- use_module(library(broker)).
:- broker_client(mylists, plbin(mylists)).

test(X) :-
    myappend([1,2,3],[4,5], X).
