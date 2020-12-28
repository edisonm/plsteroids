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

:- module(gen_bid,
          [ gen_bid/0
          ]).

:- use_module(library(filesex)).
:- use_module(library(lists)).
:- use_module(library(assertions)).
:- use_module(library(bid_desc)).

gen_bid :-
    absolute_file_name(plbin('.'), Dir, [file_type(directory), access(exist)]),
    gen_bid_pl(Dir),
    gen_bid_h(Dir).

gen_bid_pl(Dir) :-
    directory_file_path(Dir, 'bid_auto.pl', File),
    tell(File),
    dump_bid_pl,
    told.

gen_bid_h(Dir) :-
    directory_file_path(Dir, 'pl-bid_auto.h', File),
    tell(File),
    dump_bid_h,
    told.

dump_bid_h :-
    ( bid_desc(Prefix, FL, A),
      member(F, FL),
      format("GEN_BID_ALL(~w~w,~w)~n", [Prefix, A, F]),
      fail
    ; true
    ).

compats(1, T, -T) :- !.
compats(N, T, (Cs * +T)) :-
    N > 1,
    succ(N1, N),
    compats(N1, T, Cs).

compats(pl_, A, T, Cs) :- compats(A, T, Cs).
compats(pn_, A, T, Cs) :- compats(A, T, Cs).
compats(pt_, 2, T, +T * -).
compats(pi_, 2, T, -int* +T).
compats(is_, 2, T, +T* +T).

dump_bid_pl :-
    ( member(Pre-T, [bid64-bid64_t, bid128-bid128_t]),
      bid_desc(Prefix, FL, A),
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
