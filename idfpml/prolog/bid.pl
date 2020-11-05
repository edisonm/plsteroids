/*  CLP over binary integer decimal numbers

    Author:        Edison Mera Menendez
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2020, Process Design Center, Breda, The Netherlands.
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

:- module(bid,
          [bid64/2,
           bid128/2,
           bid64_t/1,
           bid128_t/1
          ]).

% Implementation using terms '$bid64'(INT64), '$bid128'(INT64, INT64)

% Original library downloaded from:
% https://software.intel.com/en-us/articles/intel-decimal-floating-point-math-library

% The library was compiled with:
% cd idfpml/LIBRARY ; make CC=gcc CALL_BY_REF=1 GLOBAL_RND=1 GLOBAL_FLAGS=1 UNCHANGED_BINARY_FLAGS=0

:- use_module(library(assertions)).
:- use_module(library(plprops)).
:- use_module(library(foreign/foreign_interface)).
:- use_module(library(foreign/foreign_props)).
% Note: We will let the parameters as default
:- extra_compiler_opts('-DDECIMAL_CALL_BY_REFERENCE=1 -DDECIMAL_GLOBAL_ROUNDING=1 -DDECIMAL_GLOBAL_EXCEPTION_FLAGS=1 -fPIC').
:- library_foreign_dir(idfpml/'LIBRARY').
:- include_foreign_dir(idfpml/'LIBRARY'/src).
:- link_foreign_library(bid).
:- use_foreign_header('pl-bid').
:- use_foreign_source('pl-bid').
:- gen_foreign_library(plbin(bid)).
:- use_module(library(gen_bid)).
:- gen_bid.

:- type [ bid64_t/1,
          bid128_t/1
        ] + native(prefix(is_)).

:- pred [ bid64/2,
          bid128/2
        ] + native(prefix(pl_)).

:- include(bid_auto).

user:portray('$bid64'(V)) :-
    bid64_string('$bid64'(V), S),
    format("~s", [S]).
user:portray('$bid128'(V1,V2)) :-
    bid128_string('$bid128'(V1,V2), S),
    format("~s", [S]).
