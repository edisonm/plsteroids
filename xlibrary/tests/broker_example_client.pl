:- module(broker_example_client,
          [ init_client/0,
            bect/1
          ]).

:- use_module(library(broker_rt)).
:- use_module(plbin(mylists_intf)).
:- use_module(plbin(mylists_locl), []). % load this only once per project
:- use_module(plbin(mylists_remt), []). % load this only once per project
:- use_module(plbin(mylists_prxy), []). % load this only once per project
:- init_expansors.

init_client :-
    add_server(mylists, 'ws://localhost:3333/broker'),
    bind_client(mylists).

bect(X) :-
    myappend([1,2,3],[4,5], X).
