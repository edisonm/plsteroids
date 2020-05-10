:- module(plsloader,
          [packages/1,
           scanpacks/1,
           scanpacks/2,
           pack_set_path/1,
           pack_load_local/3]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(pairs)).
:- use_module(library(prolog_source)).
:- use_module(library(sort)).

:- meta_predicate scanpacks(+, 1).

:- thread_local pack_path_local/1.
user:file_search_path(pltool, plroot(Pack)) :- pack_path_local(Pack).

packages(Packages) :- findall(Package, package(Package), Packages).

scanpack(Action, Pack) :-
    with_mutex(Pack, scanpacks([Pack], Action)).

scanpacks(PackL, Action) :-
    deppacks(PackL, DepPackL),
    maplist(Action, DepPackL).

requires(PackReqU1, Pack1, Pack2) :-
    once(select(Pack1-ReqL, PackReqU1, PackReqU)),
    member(Pack3, ReqL),
    ( Pack3 = Pack2
    ->true
    ; requires(PackReqU, Pack3, Pack2)
    ).

requires(PackReqU, Comp, Pack1, Pack2) :-
    ( requires(PackReqU, Pack1, Pack2)
    ->Comp = (>)
    ; Comp = (<)
    ).

deppacks(PackL, PackS) :-
    deppacks(PackL, [], PackReqU),
    pairs_keys(PackReqU, PackU),
    predsort(requires(PackReqU), PackU, PackS).

scanpacks(Action) :-
    packages(PackL),
    deppacks(PackL, DepPackL),
    concurrent_maplist(scanpack(Action), DepPackL).

deppacks(PackL) --> foldl(scanpack, PackL).

scanpack(Pack, Loaded, Loaded) :-
    memberchk(Pack-_, Loaded), !.
scanpack(Pack, Loaded1, Loaded) :-
    absolute_file_name(Pack/pack, F, [file_type(prolog)]),
    read_file(F, PackOptions),
    findall(ReqPack, member(requires(ReqPack), PackOptions), ReqPacks),
    deppacks(ReqPacks, [Pack-ReqPacks|Loaded1], Loaded).

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
    directory_source_files(plroot(Pack/prolog), Files, [recursive(true),if(true)]),
    forall(( member(File, Files),
             % \+ module_property(_, file(File)),
             \+ ( member(ExFile, ExFiles),
                  absolute_file_name(ExFile, File, [file_type(prolog), file_errors(fail)])
                )
           ), call(Loader, File)).

pack_load_local(Options, Loader, Pack) :-
    pack_set_local_path(Pack),
    pack_load_files(Options, Loader, Pack).
