/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2015, Process Design Center, Breda, The Netherlands.
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
:- use_module(library(compound_expand)).
:- use_module(library(change_alias)).
:- reexport(library(interface)).

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
    absolute_file_name(Alias, File, [file_type(prolog),
                                     relative_to(RelTo)]).

generate_broker(Target, File, Module, AliasTo) -->
    { prolog_load_context(source, RelTo),
      atom_concat(Module, '_intf', Interface),
      atom_concat(Module, '_remt', ImplRemote),
      atom_concat(Module, '_locl', ImplLocal)
    },
    generate_interface(     File, RelTo, Module, Interface,  AliasTo, AliasIntf),
    generate_local( Target, File, RelTo, Module, ImplLocal,  AliasTo, AliasIntf),
    generate_remote(Target, File, RelTo, Module, ImplRemote, AliasTo, AliasIntf),
    generate_server(Target, File, RelTo, Module, Interface,  AliasTo),
    {bind_implementation(Target, ImplLocal, ImplRemote, BindModule)},
    [(:- initialization(bind_interface(Interface, BindModule)))].

bind_implementation(client, _, M, M).
bind_implementation(server, M, _, M).
bind_implementation(proxy,  _, M, M).
bind_implementation(A,B,C,D) :- writeln(user_error, bind_implementation(A,B,C,D)),fail.

generate_interface(File, RelTo, Module, Interface, AliasTo, AliasIntf) -->
    {generate_file('_intf', File, RelTo, AliasTo, AliasIntf, dump_interface(Module, Interface))},
    [(:- use_module(AliasIntf))].

generate_local(Target, _, _, _, _, _, _) -->
    {member(Target, [client, proxy])},
    neck.
generate_local(server, File, RelTo, Module, ImplLocal, AliasTo, AliasIntf) -->
    generate_file('_locl', File, RelTo, AliasTo, _, dump_local(Module, ImplLocal, AliasIntf)).

generate_remote(server, _, _, _, _, _, _) --> [].
generate_remote(Target, File, RelTo, Module, ImplRemote, AliasTo, AliasIntf) -->
    {member(Target, [client, proxy])},
    neck,
    generate_file('_remt', File, RelTo, AliasTo, _, dump_remote(Module, ImplRemote, AliasIntf)).

generate_server(client, _, _, _, _, _) --> [].
generate_server(Target, File, RelTo, Module, Interface, AliasTo) -->
    {member(Target, [server, proxy])},
    neck,
    generate_file('_serv', File, RelTo, AliasTo, _, dump_server(Module, Interface)).

generate_file(Suffix, File, RelTo, AliasIntf, AliasSuff, Dump) :-
    change_alias(add_suffix(Suffix), AliasIntf, AliasSuff),
    alias_file(RelTo, AliasSuff, FileSuff),
    ( is_newer(FileSuff, File)
    ->true
    ; save_to_file(FileSuff, Dump)
    ).

generate_file(Suffix, File, RelTo, AliasIntf, AliasSuff, Dump) -->
    {generate_file(Suffix, File, RelTo, AliasIntf, AliasSuff, Dump)},
    [(:- use_module(AliasSuff, []))].

dump_interface(Module, Interface) -->
    [(:- module(Interface, [])),
     (:- use_module(library(interface))),
     (:- interfaces_mod(Module))].

dump_local(Module, ImplLocal, AliasIntf) -->
    [(:- module(ImplLocal, [])),
     (:- use_module(library(interface))),
     (:- implements(AliasIntf)),
     (:- module_property(Module, file(File)),
         reexport(File))].

dump_remote(Module, ImplRemote, AliasIntf) -->
    [(:- module(ImplRemote, [])),
     (:- use_module(library(interface))),
     (:- use_module(library(broker_rt))),
     (:- implements(AliasIntf))
    ],
    findall((H :- remote_call(H, Module)),
            ( module_property(Module, exports(PIL)),
              member(F/A, PIL),
              functor(H, F, A)
            )).

dump_server(Module, Interface) -->
    {atom_concat(Module, '_serv', ImplService)},
    [(:- module(ImplService, [])),
     (:- use_module(library(http/http_dispatch))),
     (:- use_module(library(http/websocket))),
     (:- use_module(library(interface))),
     (:- use_module(library(broker_ws))),
     (:- http_handler(root(broker/Module),
                      http_upgrade_to_websocket(broker_ws(Interface), []),
                      [spawn([])]))].

term_expansion_broker(Target, Alias, AliasIntf) -->
    { use_module(Alias, []), % Ensure that the module is loaded
      absolute_file_name(Alias, File, [file_type(prolog), access(read)]),
      module_property(Module, file(File))
    },
    generate_broker(Target, File, Module, AliasIntf).

term_expansion((:- broker_client(Alias, AliasIntf)), Clauses) :-
    phrase(term_expansion_broker(client, Alias, AliasIntf), Clauses).
term_expansion((:- broker_server(Alias, AliasIntf)), Clauses) :-
    phrase(term_expansion_broker(server, Alias, AliasIntf), Clauses).
term_expansion((:- broker_proxy( Alias, AliasIntf)), Clauses) :-
    phrase(term_expansion_broker(proxy,  Alias, AliasIntf), Clauses).
