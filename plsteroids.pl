% set directories
set_plroot :-
    source_location(File, _),
    directory_file_path(Dir, _, File),
    retractall(user:file_search_path(plroot, _)),
    assertz(user:file_search_path(plroot, Dir)).

file_search_path(library,    pltool(prolog)).
file_search_path(pltool,     plroot(assertions)).
file_search_path(pltool,     plroot(rtchecks)).
file_search_path(pltool,     plroot(xlibrary)).
file_search_path(pltool,     plroot(xtools)).
file_search_path(pltool,     plroot(refactor)).
file_search_path(assertions, plroot(assertions/prolog)).
file_search_path(rtchecks,   plroot(rtchecks/prolog)).
file_search_path(xlibrary,   plroot(xlibrary/prolog)).
file_search_path(xtools,     plroot(xtools/prolog)).
file_search_path(refactor,   plroot(refactor/prolog)).

:- set_plroot.

:- use_module(library(record_locations), []).
% load tools
:- use_module(library(assertions)).
:- use_module(library(rtchecks_tracer)).
% :- use_module(library(assrt_meta)).
:- use_module(library(refactor)).
:- use_module(library(audits)).
:- use_module(library(ws_cover)).

/*
:- use_module(library(prolog_stack)).
:- multifile
	user:prolog_exception_hook/4.
:- dynamic
	user:prolog_exception_hook/4.

skip_error(missing(undecided_call)).

user:prolog_exception_hook(Error, _, _, _) :-
    \+ skip_error(Error),
    format(user_error, '~q~n', [Error]),
    backtrace(20),
    fail.
*/

% user:prolog_trace_interception(Port, Frame, PC, continue).
