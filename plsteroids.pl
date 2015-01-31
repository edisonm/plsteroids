% set directories
set_plroot :-
    working_directory(WD, WD),
    retractall(user:file_search_path(plroot, _)),
    assertz(user:file_search_path(plroot, WD)).

file_search_path(library, pltool(prolog)).
file_search_path(pltool,  plroot(assertions)).
file_search_path(pltool,  plroot(rtchecks)).
file_search_path(pltool,  plroot(xlibrary)).
file_search_path(pltool,  plroot(xtools)).
file_search_path(pltool,  plroot(refactor)).

:- set_plroot.

% Warning: this should be after set_plroot to let ciao:push_ciao_library works:
:- use_module(library(dialect)).
:- use_module(library(dialect/ciao), []).

:- use_module(library(record_locations)).
% load tools
:- use_module(library(swi/assertions)).
:- use_module(library(swi/rtchecks)).
:- use_module(library(refactor)).
:- use_module(library(audits)).
:- use_module(library(ws_cover)).
/*
user:prolog_exception_hook(Error, _, _, _) :-
    format(user_error, '~q~n', [Error]),
    backtrace(20),
    fail.
*/
