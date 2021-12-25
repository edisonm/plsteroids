:- module(broker_example_client, [init/0, test/1]).

:- use_module(library(broker)).
:- broker_client(mylists, plbin(mylists)).

init :-
    add_server(mylists, 'ws://localhost:3333/broker').

test(X) :-
    myappend([1,2,3],[4,5], X).
