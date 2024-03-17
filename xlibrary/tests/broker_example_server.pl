:- module(broker_example_server, []).

:- use_module(library(broker)).
:- init_expansors.

:- broker_server(library('../tests/mylists'), plbin(mylists)).
