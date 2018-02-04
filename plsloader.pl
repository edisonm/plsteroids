:- module(plsloader,
          [packages/1,
           scanpacks/2,
           pack_set_path/2,
           pack_load_files/3,
           pack_load/3]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(prolog_source)).

:- meta_predicate scanpacks(+, 2).

packages(Packages) :- findall(Package, package(Package), Packages).

scanpacks(PackL, Action) :-
    scanpacks(PackL, Action, [], _).

scanpacks(PackL, Action) -->
    foldl(scanpack(Action), PackL).

scanpack(_, Pack, Loaded, Loaded) :-
    memberchk(Pack, Loaded), !.
scanpack(M:Action, Pack, Loaded1, Loaded) :-
    absolute_file_name(Pack/pack, F, [file_type(prolog)]),
    read_file(F, PackOptions),
    call(M:Action, Pack, M),
    findall(ReqPack, member(requires(ReqPack), PackOptions), ReqPacks),
    scanpacks(ReqPacks, M:Action, [Pack|Loaded1], Loaded).

read_file(F, Terms) :-
    see(F),
    findall(Term,
            ( repeat,
                read_term(current_input, Term, []),
                ( Term==end_of_file
                ->!,
                  fail
                ; true
                )
            ), Terms),
    seen.

pack_set_path(Pack, _) :-
    assertz(user:file_search_path(pltool, plroot(Pack))).

pack_load_files(Options, Pack, M) :-
    option(exclude(ExFiles), Options, []),
    directory_source_files(Pack/prolog, Files, [recursive(true),if(false)]),
    forall(( member(File, Files),
             \+ module_property(_, file(File)),
             \+ ( member(ExFile, ExFiles),
                  absolute_file_name(ExFile, File, [file_type(prolog), file_errors(fail)])
                )
           ), M:[File]).

pack_load(Options, Pack, M) :-
    pack_set_path(Pack, M),
    pack_load_files(Options, Pack, M).
