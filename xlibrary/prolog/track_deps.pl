/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2022, Process Design Center, Breda, The Netherlands.
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

:- module(track_deps,
          [ track_deps/5,
            collect_head_calls/2,
            head_calls_hook/5
          ]).

:- use_module(library(in_module_file)).
:- use_module(library(static_strip_module)).
:- reexport(library(compound_expand)).
:- init_expansors.

:- thread_local
    head_calls_hook_db/5.

%!  head_calls_hook(Head, Module, Body, File, Line)
%
%   Used to keep information that could be partially evaluated.
%
%  @tbd: it should be stored out of the executable, since its information is
%  only relevant to the static analyzers

:- multifile head_calls_hook/5.

track_deps(File, Line, M, Head, Body) :-
    % Help static analysis to keep track of dependencies. TBD: find a
    % way to store this out of the executable, for instance, in an asr file
    freeze(Head,
           ( static_strip_module(Head, M, Pred, MH),
             freeze(
                 MH,
                 freeze(
                     Pred,
                     assertz(head_calls_hook_db(Pred, MH, Body, File, Line)))))).

collect_head_calls -->
    findall(track_deps:head_calls_hook(Head, M, Body, File, Line),
            retract(head_calls_hook_db(Head, M, Body, File, Line))).

term_expansion(end_of_file, ClauseL) :-
    in_module_file,
    collect_head_calls(ClauseL, [end_of_file]).
