:- use_module(library(dialect)).
:- use_module(library(dialect/ciao), []).

% set directories
set_plroot :-
    working_directory(WD, WD),
    retractall(user:file_search_path(plroot, _)),
    asserta(user:file_search_path(plroot, WD)).

:- set_plroot.
    
file_search_path(library, pltool(prolog)).
file_search_path(pltool,  plroot(assertions)).
file_search_path(pltool,  plroot(rtchecks)).
file_search_path(pltool,  plroot(xlibrary)).
file_search_path(pltool,  plroot(xtools)).
file_search_path(pltool,  plroot(refactor)).

% load tools
:- use_module(library(refactor)).
:- use_module(library(audits)).
:- use_module(library(ws_cover)).
