/*  CLP for multiple precision floating-point computation

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

:- module(gen_floatn,
          [ gen_floatn/0
          ]).

:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(neck)).
:- use_module(library(assertions)).
:- use_module(library(floatn_desc)).

gen_floatn :-
    absolute_file_name(library(gen_floatn), This, [file_type(prolog), access(exist)]),
    directory_file_path(Dir, _, This),
    gen_floatn_pl(Dir),
    gen_floatn_h(Dir).

gen_floatn_pl(Dir) :-
    directory_file_path(Dir, 'floatn_auto.pl', File),
    tell(File),
    dump_floatn_pl,
    told.

gen_floatn_h(Dir) :-
    directory_file_path(Dir, 'pl-floatn_auto.h', File),
    tell(File),
    dump_floatn_h,
    told.

dump_floatn_h :-
    ( floatn_desc(Prefix, FL, A),
      member(F, FL),
      format("GEN_FLOATN_ALL(~w~w,~w)~n", [Prefix, A, F]),
      fail
    ; true
    ).

compats(1, T, int * -T) :- !.
compats(N, T, (Cs * +T)) :-
    N > 1,
    succ(N1, N),
    compats(N1, T, Cs).

compats(Prefix, A1, T, Cs) :-
    member(Prefix, [pl_, pc_]),
    neck,
    succ(A, A1), compats(A, T, Cs).
compats(pi_, 4, T, int * -T * +int * +T).
compats(ip_, 4, T, int * -T * +T * +int).
compats(is_, 1, T, +T).
compats(is_, 2, T, +T * +T).

dump_floatn_pl :-
    ( member(Pre-T, [floatn-floatn_t]),
      floatn_desc(Prefix, FL, A),
      compats(Prefix, A, T, Cs),
      findall(Func/A,
              ( member(F, FL),
                atomic_list_concat([Pre, '_', F], Func)
              ), PIL),
      forall(member(PI, PIL), portray_clause((:- export(PI)))),
      portray_clause((:- pred PIL :: Cs + native(prefix(Prefix)))),
      fail
    ; true
    ).
