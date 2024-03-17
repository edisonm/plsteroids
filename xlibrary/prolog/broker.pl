/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2021, Process Design Center, Breda, The Netherlands.
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

:- module(broker, []).

:- use_module(library(lists)).
:- use_module(library(neck)).
:- use_module(library(codegen)).
:- use_module(library(change_alias)).
:- reexport(library(compound_expand)).
:- reexport(library(interface)).
:- init_expansors.

/** <module> WebSocket broker

WebSocket broker that allows to execute predicates remotely.

    fallback(local)
    timeout
    timereset

balancing
  roundrobin
  leastconec

*/

alias_file(RelTo, Alias, File) :-
    absolute_file_name(Alias, File, [file_type(prolog), relative_to(RelTo)]).

generate_server(File, Alias, Module, AliasTo) -->
    { prolog_load_context(source, RelTo),
      atom_concat(Module, '_intf', Interface),
      atom_concat(Module, '_remt', ImplRemote),
      atom_concat(Module, '_locl', ImplLocal),
      generate_intf(File, RelTo, Module, Interface,  AliasTo, AliasIntf),
      generate_locl(File, RelTo, Alias,  ImplLocal,  AliasTo, AliasIntf),
      generate_remt('_remt', File, RelTo, Module, ImplRemote, AliasTo, AliasIntf),
      generate_serv(File, RelTo, Module, AliasTo, AliasServ)
    },
    [(:- use_module(AliasServ, []))].

generate_proxy(ImplProxy, File, Module, AliasIntf, AliasTo) -->
    { prolog_load_context(source, RelTo),
      generate_remt('', File, RelTo, Module, ImplProxy, AliasTo, AliasIntf),
      generate_prxy(File, RelTo, Module, ImplProxy, AliasTo, AliasSuff)
    },
    [(:- use_module(AliasSuff, []))].

generate_intf(File, RelTo, Module, Interface, AliasTo, AliasIntf) :-
    generate_file('_intf', File, RelTo, AliasTo, AliasIntf, dump_interface(Module, Interface)).

generate_locl(File, RelTo, Alias, ImplLocal, AliasTo, AliasIntf) :-
    generate_file('_locl', File, RelTo, AliasTo, _, dump_local(Alias, ImplLocal, AliasIntf)).

generate_remt(Suff, File, RelTo, Module, ImplRemote, AliasTo, AliasIntf) :-
    generate_file(Suff, File, RelTo, AliasTo, _, dump_remote(Module, ImplRemote, AliasIntf)).

generate_prxy(File, RelTo, Module, ImplRemote, AliasTo, AliasSuff) :-
    atom_concat(ImplRemote, '_serv', ModuleServ),
    generate_file('_serv', File, RelTo, AliasTo, AliasSuff, dump_server(Module, ModuleServ)).

generate_serv(File, RelTo, Module, AliasTo, AliasSuff) :-
    atom_concat(Module, '_serv', ModuleServ),
    generate_file('_serv', File, RelTo, AliasTo, AliasSuff, dump_server(Module, ModuleServ)).

:- meta_predicate
    generate_file(+,+,+,+,+,2).

generate_file(Suffix, File, RelTo, AliasIntf, AliasSuff, Dump) :-
    change_alias(add_suffix(Suffix), AliasIntf, AliasSuff),
    alias_file(RelTo, AliasSuff, FileSuff),
    ( is_newer(FileSuff, File)
    ->true
    ; term_to_file(FileSuff, Dump)
    ).

dump_interface(Module, Interface) -->
    {module_property(Module, exports(PIL))},
    [ (:- module(Interface, PIL)),
      (:- use_module(library(interface))),
      (:- init_expansors)
    ],
    end_interface(Interface, PIL).

dump_local(Alias, ImplLocal, AliasIntf) -->
    [ (:- module(ImplLocal, [])),
      (:- use_module(library(interface))),
      (:- reexport(Alias)),
      (:- init_expansors),
      (:- implements(AliasIntf))
    ].

dump_remote(Module, ImplRemote, AliasIntf) -->
    [ (:- module(ImplRemote, [])),
      (:- use_module(library(interface))),
      (:- use_module(library(broker_rt))),
      (:- init_expansors),
      (:- implements(AliasIntf)),
      broker_rt:'$broker'(Module)
    ],
    findall((H :- remote_call(H, Module)),
            ( module_property(Module, exports(PIL)),
              member(F/A, PIL),
              functor(H, F, A)
            )).

dump_server(Module, ImplService) -->
    [ (:- module(ImplService, [])),
      (:- use_module(library(http/http_dispatch))),
      (:- use_module(library(http/websocket))),
      (:- use_module(library(broker_ws))),
      (:- init_expansors),
      (:- http_handler(root(broker/Module),
                       http_upgrade_to_websocket(broker_ws(Module), []),
                       [spawn([])]))
    ].

term_expansion_proxy(ImplRemote, AliasIntf, AliasImpl, AliasPrxy) -->
    { prolog_load_context(source, RelTo),
      absolute_file_name(AliasImpl, File, [file_type(prolog), access(read), relative_to(RelTo)]),
      use_module(AliasImpl, []), % Ensure that the module is loaded
      module_property(Module, file(File))
    },
    generate_proxy(ImplRemote, File, Module, AliasIntf, AliasPrxy).

term_expansion_server(Alias, AliasIntf) -->
    { prolog_load_context(source, RelTo),
      absolute_file_name(Alias, File, [file_type(prolog), access(read), relative_to(RelTo)]),
      use_module(Alias, []), % Ensure that the module is loaded
      module_property(Module, file(File))
    },
    generate_server(File, Alias, Module, AliasIntf).

term_expansion((:- broker_server(Alias, AliasIntf)), Clauses) :-
    phrase(term_expansion_server(Alias, AliasIntf), Clauses).
term_expansion((:- broker_proxy(ImplRemote, AliasIntf, AliasImpl, AliasPrxy)), Clauses) :-
    phrase(term_expansion_proxy(ImplRemote, AliasIntf, AliasImpl, AliasPrxy), Clauses).
