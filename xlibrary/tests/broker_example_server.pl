:- module(broker_example_server,
          [ start_wsbroker/0
          ]).

:- use_module(library(broker)).
:- use_module(broker_example_common).
:- broker_server(mylists, plbin(mylists)).

start_wsbroker :- start_wsbroker(4444).
