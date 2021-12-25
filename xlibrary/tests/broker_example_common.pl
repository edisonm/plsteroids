:- module(broker_example_common,
          [ start_wsbroker/1
          ]).
:- use_module(library(main)).
:- use_module(library(option)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_unix_daemon)).
:- use_module(library(lists)).

start_wsbroker(Port) :-
    current_prolog_flag(argv, Argv),
    argv_options(Argv, _, Options1),
    merge_options(Options1, [port(Port)], Options),
    ( option(daemon(true), Options)
    ->http_daemon(Options)
    ; http_server(http_dispatch, Options)
    ).
