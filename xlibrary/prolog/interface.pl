/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2014, Process Design Center, Breda, The Netherlands.
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

:- module(interface,
          [ bind_interface/2
          ]).

:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(error)).
:- reexport(library(compound_expand)).
:- init_expansors.

:- multifile
    '$interface_spec'/2,
    '$interface'/2,
    '$implementation'/2.

% Warning: an interface module can not contain dynamic predicates, since dynamic
% predicates are generated automatically to perform the binding

not_interface(M, F/A) :-
    current_predicate(M:F/A),
    functor(H, F, A),
    \+ predicate_property(M:H, dynamic),
    \+ predicate_property(M:H, imported_from(_)).

this_interface(Interface, DIL) -->
    [interface:'$interface'(Interface, DIL)].

scope_decl(shared, F/A, (:- dynamic F/A)).
scope_decl(local,  F/A, (:- thread_local F/A)).

decl_dynbridge(DIL, Scope) -->
    {scope_decl(Scope, F/A, Decl)},
    findall(Decl, member(F/A, DIL)).

end_interface(Interface, Scope, DIL) -->
    this_interface(Interface, DIL),
    decl_dynbridge(DIL, Scope).

term_expansion_decl(implements(Alias), Clauses) :-
    '$current_source_module'(Implementation),
    Implementation:use_module(Alias, []), % Ensure that the module is loaded
    absolute_file_name(Alias, File, [file_type(prolog), access(read)]),
    module_property(Interface, file(File)),
    term_expansion_decl(implements_mod(Interface), Clauses).
term_expansion_decl(implements_mod(Interface), Clauses) :-
    '$current_source_module'(Implementation),
    '$interface'(Interface, PIL),
    phrase(( [interface:'$implementation'(Implementation, Interface)],
             findall((:- meta_predicate Implementation:Spec),
                     ( member(F/A, PIL),
                       functor(Pred, F, A),
                       predicate_property(Interface:Pred, meta_predicate(Spec))
                     )),
             findall((:- export(PI)), member(PI, PIL))
           ), Clauses).
term_expansion_decl(interfaces(Alias), Clauses) :-
    term_expansion_decl(interfaces(Alias, shared), Clauses).
term_expansion_decl(interfaces(Alias, Scope), Clauses) :-
    '$current_source_module'(Interface),
    Interface:use_module(Alias, []),
    absolute_file_name(Alias, File, [file_type(prolog), access(read)]),
    module_property(Implementation, file(File)),
    term_expansion_decl(interfaces_mod(Implementation, Scope), Clauses).
term_expansion_decl(interfaces_mod(Implementation), Clauses) :-
    term_expansion_decl(interfaces_mod(Implementation, shared), Clauses).
term_expansion_decl(interfaces_mod(Implementation, Scope), Clauses) :-
    '$current_source_module'(Interface),
    phrase(interfaces_mod_clauses(Interface, Scope, Implementation), Clauses).
term_expansion_decl(interface, interface:'$interface_spec'(Interface, shared)) :- 
    '$current_source_module'(Interface).
term_expansion_decl(interface(Scope), interface:'$interface_spec'(Interface, Scope)) :-
    memberchk(Scope, [shared, local]),
    '$current_source_module'(Interface).

interfaces_mod_clauses(Interface, Scope, Implementation) -->
    {module_property(Implementation, exports(PIL))},
    findall((:- export(PI)), member(PI, PIL)),
    end_interface(Interface, Scope, PIL).

term_expansion((:- Decl), Clauses) :-
    term_expansion_decl(Decl, Clauses).
term_expansion(end_of_file, Clauses) :-
    '$current_source_module'(Interface),
    '$interface_spec'(Interface, Scope),
    module_property(Interface, file(File)),
    prolog_load_context(source, File),
    module_property(Interface, exports(PIL)),
    exclude(not_interface(Interface), PIL, DIL),
    phrase(end_interface(Interface, Scope, DIL), Clauses, [end_of_file]).

prolog:called_by(Pred, Interface, Context, PredL) :-
    '$interface'(Interface, DIL),
    member(F/A, DIL),
    functor(Pred, F, A),
    findall(@(Implementation:Pred, Context),
            interface:'$implementation'(Implementation, Interface),
            PredL),
    PredL \= [].

%!  bind_interface(+Interface:atom, +Implementation:atom) is semidet.
%
%   Connects Interface with Implementation.

bind_interface(Interface, Implementation) :-
    ( '$interface'(Interface, DIL)
    ->true
    ; existence_error(interface, Interface)
    ),
    ( '$implementation'(Implementation, Interface)
    ->true
    ; ( '$implementation'(Implementation, _)
      ->existence_error(implementation, Implementation)
      ; existence_error(binding, Interface->Implementation)
      )
    ),
    forall(( member(F/A, DIL),
             functor(H, F, A)
           ),
           ( retractall(Interface:H),
             Implementation:assertz((Interface:H :- H)))).
