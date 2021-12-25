/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
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

:- module(codegen,
          [ add_suffix/3,
            is_newer/2,
            line_atom/2,
            save_to_file/2,
            with_output_to_file/2
          ]).

is_newer(File1, File2) :-
    exists_file(File1),
    exists_file(File2),
    time_file(File1, Time1),
    time_file(File2, Time2),
    Time1 > Time2.

add_suffix(Suffix, Name1, Name) :-
    file_name_extension(Name2, _, Name1),
    atom_concat(Name2, Suffix, Name).

:- meta_predicate with_output_to_file(+,0 ).

with_output_to_file(File, Goal) :-
    setup_call_cleanup(tell(File), Goal, told).

:- meta_predicate save_to_file(+,2).

save_to_file(File, Goal) :-
    call(Goal, Lines, []),
    with_output_to_file(File, write_lines(Lines)).

write_lines([]) :- !.
write_lines([E|L]) :- !,
    write_lines(E),
    write_lines(L).
write_lines(Line) :-
    write_line(Line).

write_line(Line) :-
    ( nonvar(Line),
      do_write_line_2(Line)
    ->true
    ; writeln(Line)
    ).

write_line_1(Line) :-
    ( nonvar(Line),
      do_write_line_1(Line)
    ->true
    ; write(Line)
    ).

do_write_line_1(F-A) :-
    format(F, A).
do_write_line_1(A+B) :-
    write_line_1(A),
    write_line_1(B).
do_write_line_1(A/S) :-
    maplist(line_atom, [S|A], [C|L]),
    atomic_list_concat(L, C, V),
    write(V).

line_atom(Line, Atom) :- with_output_to(atom(Atom), write_line_1(Line)).

do_write_line_2((:- A))    :- portray_clause((:- A)).
do_write_line_2((A :- B))  :- portray_clause((A :- B)).
do_write_line_2((A --> B)) :- portray_clause((A --> B)).
do_write_line_2(Line) :- write_line_1(Line), nl.
