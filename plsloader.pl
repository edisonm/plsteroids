:- module(plsloader,
          [packages/1,
           scanpacks/2,
           scanpacks/3,
           pack_set_path/1,
           pack_set_local_path/1,
           pack_load_local/3]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(prolog_source)).
:- use_module(library(sort)).

:- meta_predicate
    scanpacks(1, 1),
    scanpacks(+, 1, 1).

:- thread_local pack_path_local/1.

user:file_search_path(pltool, plroot(Pack)) :- pack_path_local(Pack).

packages(Packages) :- findall(Package, package(Package), Packages).

scanpacks(Action, DepAction) :-
    packages(PackL),
    scanpacks(PackL, Action, DepAction).

scanpacks(PackL, Action, DepAction) :-
    concurrent_maplist(scanpacks(Action, DepAction, []), PackL).

scanpacks(_, _, Loaded, Pack) :-
    memberchk(Pack, Loaded),
    !,
    print_message(error, format("Circular dependency not allowed ~w", [[Pack|Loaded]])).
scanpacks(Action, DepAction, Loaded, Pack) :-
    absolute_file_name(Pack/pack, F, [file_type(prolog)]),
    read_file(F, PackOptions),
    findall(ReqPack, member(requires(ReqPack), PackOptions), PackL),
    concurrent_maplist(scanpacks(Action, DepAction, [Pack|Loaded]), PackL),
    maplist(DepAction, PackL),
    with_mutex(Pack, call(Action, Pack)).

read_file(F, Terms) :-
    setup_call_cleanup(
        open(F, read, S),
        findall(Term,
                ( repeat,
                  read_term(S, Term, []),
                  ( Term==end_of_file
                  ->!,
                    fail
                  ; true
                  )
                ), Terms),
        close(S)).

pack_set_path(Pack) :-
    assertz(user:file_search_path(pltool, plroot(Pack))).

pack_set_local_path(Pack) :-
    assertz(pack_path_local(Pack)).

pack_load_files(Options, Loader, Pack) :-
    option(exclude(ExFiles), Options, []),
    directory_source_files(plroot(Pack/prolog), Files, [recursive(true), if(true)]),
    forall(( member(File, Files),
             % \+ module_property(_, file(File)),
             \+ ( member(ExFile, ExFiles),
                  absolute_file_name(ExFile, File, [file_type(prolog), file_errors(fail)])
                )
           ), call(Loader, File)).

pack_load_local(Options, Loader, Pack) :-
    pack_set_local_path(Pack),
    pack_load_files(Options, Loader, Pack).
