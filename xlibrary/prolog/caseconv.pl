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

:- module(caseconv,
          [ camel_kebab/2,
            camel_snake/2,
            kebab_snake/2,
            pascal_kebab/2,
            pascal_snake/2
          ]).

:- use_module(library(ctypes)).

charsep_case(Char, UCFirst, CharSep, Snake) :-
    atom(CharSep),
    !,
    atom_codes(CharSep, CharSepC),
    charsep_case_c(CharSepC, Char, UCFirst, SnakeC),
    atom_codes(Snake, SnakeC).
charsep_case(Char, UCFirst, CharSep, Snake) :-
    atom(Snake),
    atom_codes(Snake, SnakeC),
    charsep_case_c(CharSepC, Char, UCFirst, SnakeC),
    atom_codes(CharSep, CharSepC).

case_first(u, U, S) :-
    ( upper_lower(U, S)
    ->true
    ; U = S
    ).

case_first(l, L, S) :-
    ( upper_lower(S, L)
    ->true
    ; L = S
    ).

charsep_case_c([U|CL], C, F, [S|SL]) :-
    case_first(F, U, S),
    charsep_case_2(CL, C, SL).
charsep_case_c([], _, _, []).

charsep_case_2([U|CL], Char, [Char, L|SL]) :-
    upper_lower(U, L),
    !,
    charsep_case_2(CL, Char, SL).
charsep_case_2([C|CL], Char, [C|SL]) :-
    charsep_case_2(CL, Char, SL).
charsep_case_2([], _, []).

kebab_snake(Kebab, Snake) :-
    atom(Kebab),
    !,
    atomic_list_concat(List, '-', Kebab),
    atomic_list_concat(List, '_', Snake).
kebab_snake(Kebab, Snake) :-
    atom(Snake),
    !,
    atomic_list_concat(List, '_', Snake),
    atomic_list_concat(List, '-', Kebab).

pascal_snake(Pascal, Snake) :- charsep_case(0'_, u, Pascal, Snake).
camel_snake( Camel,  Snake) :- charsep_case(0'_, l, Camel,  Snake).
pascal_kebab(Pascal, Kebab) :- charsep_case(0'-, u, Pascal, Kebab).
camel_kebab( Camel,  Kebab) :- charsep_case(0'-, l, Camel,  Kebab).
