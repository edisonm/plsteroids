/*  Part of Extended Libraries for Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(packloader,
          [ scanpacks/3,
            pack_load_files/4,
            pack_set_path/1,
            pack_set_local_path/1,
            pack_load_local/3,
            use_module_ne/1
          ]).

:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(prolog_source)).
:- use_module(read_file).
:- use_module(conc_forall).

/** <module> Package loader

This module facilitates the load of all the modules in a package.

*/

:- meta_predicate
    scanpacks(+, 1, 1),
    use_module_ne(:).

:- thread_local pack_path_local/1.

user:file_search_path(pltool, plroot(Pack)) :- pack_path_local(Pack).

use_module_ne(Lib) :- use_module(Lib, []).

scanpacks(PackL, Action, DepAction) :-
    conc_forall(member(P, PackL), scanpacks(Action, DepAction, [], P)).

scanpacks(_, _, Loaded, Pack) :-
    memberchk(Pack, Loaded),
    !,
    print_message(error, format("Circular dependency not allowed ~w", [[Pack|Loaded]])).
scanpacks(Action, DepAction, Loaded, Pack) :-
    absolute_file_name(Pack/pack, F, [file_type(prolog)]),
    read_file(F, PackOptions),
    findall(ReqPack, member(requires(ReqPack), PackOptions), PackL),
    conc_forall(member(P, PackL), scanpacks(Action, DepAction, [Pack|Loaded], P)),
    forall(member(P, PackL), call(DepAction, P)),
    with_mutex(Pack, call(Action, Pack)).

pack_set_path(Pack) :-
    assertz(user:file_search_path(pltool, plroot(Pack))).

pack_set_local_path(Pack) :-
    assertz(pack_path_local(Pack)).

:- meta_predicate pack_load_files(+,1,+,+).

pack_load_files(Options, Loader, Pack, Dir) :-
    option(exclude(ExFiles), Options, []),
    directory_source_files(Dir, Files, [recursive(true), if(true)]),
    atom_concat('_aux', Pack, AuxModule),
    forall(( member(File, Files),
             % \+ module_property(_, file(File)),
             \+ ( member(ExFile, ExFiles),
                  absolute_file_name(ExFile, File, [file_type(prolog), file_errors(fail)])
                )
           ), call(Loader, AuxModule:File)).

:- meta_predicate pack_load_local(+,1,+).

pack_load_local(Options, Loader, Pack) :-
    pack_set_local_path(Pack),
    pack_load_files(Options, Loader, Pack, plroot(Pack/prolog)).
