:- module(broker_example_proxy,
          [ init_proxy/0
          ]).

:- use_module(library(broker)).
:- init_expansors.

:- broker_proxy(mylists_prxy,
                plbin(mylists_intf),
                plbin(mylists_remt),
                plbin(mylists_prxy)).

init_proxy :-
    add_server(mylists, 'ws://localhost:3333/broker'),
    bind_client(mylists).
