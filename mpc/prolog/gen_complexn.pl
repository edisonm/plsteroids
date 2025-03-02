/*  CLP for multiple precision floating-point computation

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
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

:- module(gen_complexn,
          [ gen_complexn/0
          ]).

:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(neck)).
:- use_module(library(assertions)).
:- use_module(library(complexn_desc)).
:- init_expansors.

gen_complexn :-
    absolute_file_name(plbin('.'), Dir, [file_type(directory), access(exist)]),
    gen_complexn_pl(Dir),
    gen_complexn_h(Dir).

gen_complexn_pl(Dir) :-
    directory_file_path(Dir, 'complexn_auto.pl', File),
    tell(File),
    dump_complexn_pl,
    told.

gen_complexn_h(Dir) :-
    directory_file_path(Dir, 'pl-complexn_auto.h', File),
    tell(File),
    dump_complexn_h,
    told.

dump_complexn_h :-
    ( complexn_desc(Prefix, FL, A),
      member(F, FL),
      format("GEN_COMPLEXN_ALL(~w~w,~w)~n", [Prefix, A, F]),
      fail
    ; true
    ).

compats(1, T, int * int * -T) :- !.
compats(N, T, (Cs * +T)) :-
    N > 1,
    succ(N1, N),
    compats(N1, T, Cs).

compats(Prefix, A2, T, Cs) :-
    member(Prefix, [pl_, pc_]),
    necks,
    A is A2 - 2,
    compats(A, T, Cs).
% compats(pc_, 3, T, int * int * -T).
% compats(pl_, 4, T, int * int * -T * +T).
% compats(pl_, 5, T, int * int * -T * +T * +T).
compats(pi_, 3, T, -int * +T * +T).
compats(pf_, 4, T, int * int * -floatn_t * +T).
compats(pf_, 5, T, int * int * -T * +floatn_t * +floatn_t).

dump_complexn_pl :-
    ( member(Pre-T, [complexn-complexn_t]),
      complexn_desc(Prefix, FL, A),
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
