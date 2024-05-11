/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2024, Process Design Center, Breda, The Netherlands.
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

:- module(shrink,
          [ shrink/2,
            expand/2
          ]).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(neck)).
:- init_expansors.

:- meta_predicate
        shrink(1, -),
        expand(1, -).

esc_max_size(16).

% escaped_atom/1 will be inlined to improve performance

escaped_atom(B) :-
    esc_max_size(N),
    between(1, N, X),
    length(L, X),
    maplist(=(&), L),
    atomic_list_concat(L, B).

shrink(GenTerm, Shrink) :- shrink_expand(shrink, GenTerm, Shrink).

expand(GenTerm, Expand) :- shrink_expand(expand, GenTerm, Expand).

shrink_expand_args(shrink, S2, S, S2, S).
shrink_expand_args(shrnkt, S2, S, S2, S).
shrink_expand_args(expand, S2, S, S, S2).
shrink_expand_args(expndt, S2, S, S, S2).

shrink_expand(Cmd, GenTerm, S) :-
    SL = s(''),
    call(GenTerm, S2),
    SL = s(S1),
    shrink_expand_args(Cmd, S2, S, S3, S4),
    once(diff_term(Cmd, S1, S3, S4)),
    nb_setarg(1, SL, S3).

%!  skip_comp(Term)
%
%   Skip compression in some values, that could actually result in a bigger
%   result

skip_comp(T) :- var(T).
skip_comp(B) :-
    escaped_atom(B),
    necki,
    !,
    fail.
skip_comp([]).
skip_comp('[|]').
skip_comp(Atom) :-
    atom(Atom),
    current_op(_, _, Atom),
    atom_length(Atom, L),
    L =< 3.
skip_comp(Atom) :-
    atomic(Atom),
    write_length(Atom, L,
                 [ quoted(true),
                   backquoted_string(true)
                 ]),
    L =< 2.

:- meta_predicate
    diff_term(+, ?, ?, ?).

diff_term(Cmd, A, B, D) :-
    diff_term(Cmd, =@=, A, B, D).

cmd_into(shrink, shrnkt).
cmd_into(shrnkt, shrnkt).
cmd_into(expand, expndt).
cmd_into(expndt, expndt).

diff_term(_,      _, _, T, T) :- skip_comp(T), !.
diff_term(shrink, O, A, B, C) :- diff_term_into(shrink, O, A, B, C), !.
diff_term(expand, O, A, B, C) :- diff_term_into(expand, O, A, B, C), !.
diff_term(shrnkt, O, A, B, C) :- diff_term_shrnkt(O, A, B, [], C), !.
diff_term(expndt, _, A, B, C) :- diff_term_expndt(   A, B,     C), !.
diff_term(Cmd,    O, A, B, D) :- diff_term_args(Cmd, O, A, B, D), !.
diff_term(_, _, _, T, T).

diff_term_args(Cmd, _, A, X, Y) :-
    compound(A),
    A =.. L1,
    length(L1, N1),
    cmd_into(Cmd, Into),
    shrink_expand_args(Into, X, Y, B, D),
    compound(B),
    B =.. L2,
    length(L2, N2),
    N is min(N1, N2),
    length(C1, N),
    length(C2, N),
    append(C1, _,  L1),
    append(C2, R2, L2),
    shrink_expand_args(Into, C2, C3, D2, D3),
    % don't try to find a way to use =@= in inner terms, it will make this
    % module difficult to extend/maintain due to recursivity and free variables
    maplist(diff_term(Into, ==), C1, D2, D3),
    append(C3, R2, L3),
    D =.. L3,
    !.

escape_atom(B, C) :-
    escaped_atom(B),
    atom_concat(&, B, C),
    neck.

diff_term_shrnkt(_,   _, B, _, C) :- escape_atom(B, C), !.
diff_term_shrnkt(=@=, A, B, L, &) :- A-L =@= B-L.
diff_term_shrnkt(==,  A, B, L, &) :- A-L ==  B-L.

simplify_term(M:A, M:B, A, B).
simplify_term(  A,   B, A, B).

diff_term_expndt(A, A, B) :-
    B == (&).
diff_term_expndt(_, C, B) :-
    escape_atom(C, B).

diff_term_into(Cmd, O, P, Q, R) :-
    once(( var(R)
         ; R \= (_:_)
         )),
    once(simplify_term(P, Q, A, B)),
    cmd_into(Cmd, Into),
    diff_term(Into, O, A, B, R).
