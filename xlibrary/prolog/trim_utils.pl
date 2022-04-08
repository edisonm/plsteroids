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

:- module(trim_utils,
          [left_trim/2,
           right_trim/2,
           trim/2,
           atom_left_trim/2,
           atom_right_trim/2,
           atom_trim/2,
           string_left_trim/2,
           string_right_trim/2,
           string_trim/2
           ]).

:- use_module(library(pcre)).

left_trim(Type, Atomic, LeftTrim) :-
    % Can get valid spaces using char_type(A, space), except special ones
    re_replace("^[ \t\n\v\f\r]+", "", Atomic, LeftTrim, [capture_type(Type)]).

right_trim(Type, Atomic, RightTrim) :-
    re_replace("[ \t\n\v\f\r]+$", "", Atomic, RightTrim, [capture_type(Type)]).

trim(Type, Atomic, Trim) :-
    left_trim(Type, Atomic, LTrim),
    right_trim(Type, LTrim, Trim).

atom_left_trim(Atom, LeftTrim) :-
    left_trim(atom, Atom, LeftTrim).

atom_right_trim(Atom, RightTrim) :-
    right_trim(atom, Atom, RightTrim).

atom_trim(Atom, Trim) :-
    trim(atom, Atom, Trim).

string_left_trim(String, LeftTrim) :-
    left_trim(string, String, LeftTrim).

string_right_trim(String, RightTrim) :-
    right_trim(string, String, RightTrim).

string_trim(String, Trim) :-
    trim(string, String, Trim).

left_trim(Codes, LTrim) :-
    string_codes(String, Codes),
    string_left_trim(String, STrim),
    string_codes(STrim, LTrim).

right_trim(Codes, RTrim) :-
    string_codes(String, Codes),
    string_right_trim(String, STrim),
    string_codes(STrim, RTrim).

trim(Codes, Trim) :-
    string_codes(String, Codes),
    string_trim(String, STrim),
    string_codes(STrim, Trim).
