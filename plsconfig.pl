:- module(plsconfig, []).

:- use_module(library(filesex)).
% set directories
set_plroot :-
    source_location(File, _),
    directory_file_path(Dir, _, File),
    % directory_file_path(Root, _, File),
    directory_file_path(Dir, target, BinDir),
    retractall(user:file_search_path(plroot, _)),
    ( exists_directory(BinDir)
    ->assertz(user:file_search_path(plroot, BinDir)),
      print_message(information, format("Using binary files", []))
    ; assertz(user:file_search_path(plroot, Dir)),
      print_message(information, format("Using source files", []))
    ).

user:file_search_path(library, pltool(prolog)).

:- set_plroot.
