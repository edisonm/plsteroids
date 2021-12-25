:- module(broker_example_proxy,
          [ init/0,
            start_wsbroker/0
          ]).

:- use_module(library(broker)).
:- use_module(broker_example_common).
:- broker_proxy(mylists, plbin(mylists)).

init :-
    add_server(mylists, 'ws://localhost:4444/broker').

start_wsbroker :- start_wsbroker(3333).
