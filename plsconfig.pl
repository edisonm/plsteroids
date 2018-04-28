:- module(plsconfig, []).

:- use_module(library(filesex)).
% set directories
set_plroot :-
    source_location(File, _),
    directory_file_path(Dir, _, File),
    retractall(user:file_search_path(plroot, _)),
    assertz(user:file_search_path(plroot, Dir)).

user:file_search_path(library, pltool(prolog)).

:- set_plroot.
