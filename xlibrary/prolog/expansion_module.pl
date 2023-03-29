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

:- module(expansion_module,
          [ expanded_module/2,
            expansion_module/2,
            is_expansion_module/1
          ]).

:- use_module(library(option)).

:- thread_local em_db/1.

%!  expansion_module(+Module, ExpansionModule)
%
%   Kludge: using swipl internals. Perhaps is not a good idea --EMM
%
expansion_module(M, EM) :-
    setup_call_cleanup(
        assertz(em_db(M)),
        ( '$load_context_module'(File, M, _),
          module_property(CM, file(File)),
          expansion_module_rec(CM, EM)
        ),
        retractall(em_db(_))).

expansion_module_rec(M, EM) :-
    \+ em_db(M),
    assertz(em_db(M)),
    current_op(1, fx, M:'$compound_expand'),
    '$load_context_module'(File, M, Opts),
    module_property(CM, file(File)),
    ( CM = compound_expand
    ->EM = M
    ; option(reexport(true), Opts),
      expansion_module_rec(CM, EM)
    ).

is_expansion_module(EM) :-
    current_op(1, fx, EM:'$compound_expand'),
    '$load_context_module'(File, EM, Opts),
    option(reexport(true), Opts),
    module_property(CM, file(File)),
    current_op(1, fx, CM:'$compound_expand'),
    !.

expanded_module(EM, M) :-
    is_expansion_module(EM),
    expanded_module_rec(EM, M).

expanded_module_rec(EM, M) :-
    module_property(EM, file(File)),
    '$load_context_module'(File, CM, Opts),
    ( M = CM
    ; option(reexport(true), Opts),
      current_op(1, fx, CM:'$compound_expand'),
      expanded_module_rec(CM, M)
    ).
