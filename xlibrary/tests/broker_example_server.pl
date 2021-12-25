:- module(broker_example_server,
          [ start_wsbroker/0
          ]).

:- use_module(library(broker)).
:- broker_server(mylists, plbin(mylists)).

:- use_module(library(main)).
:- use_module(library(option)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(lists)).

start_wsbroker :-
    current_prolog_flag(argv, Argv),
    argv_options(Argv, _, Options1),
    merge_options(Options1, [port(3333)], Options),
    ( option(daemon(true), Options)
    ->http_daemon(Options)
    ; http_server(http_dispatch, Options)
    ).
