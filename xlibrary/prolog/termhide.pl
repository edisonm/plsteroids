/*  Part of Extended Libraries for Prolog

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

:- module(termhide, [op(1150, fx, hide)]).

:- use_module(library(sequence_list)).
:- use_module(library(occurs)).
:- reexport(library(compound_expand)).
:- init_expansors.

/* <module> Term hide

Inspired in the declaration ```:- hide Spec``` as described in the paper:

N. Stulova, J. F. Morales, M. V. Hermenegildo. Towards Run-time Checks
Simplification via Term Hiding. Technical Communications of the 33rd
International Conference on Logic Programming (ICLP 2017), OpenAccess Series in
Informatics (OASIcs), Vol. 58, pages 91-93, Schloss Dagstuhl-Leibniz-Zentrum
fuer Informatik, 2017. (Extended Abstract).

But note that the motivation to implement this is different than the one in such
paper.  What we want is to avoid irrestricted access to data structures out of a
given module.
*/

:- multifile '$termhide'/2.

term_expansion_decl(hide(PIS), ClauseL) :-
    prolog_load_context(module, M),
    sequence_list(PIS, List, []),
    findall(termhide:'$termhide'(H, M),
            ( member(F/A, List),
              functor(H, F, A)
            ), ClauseL).

scan(Scope, Goal) :-
    prolog_load_context(module, C),
    ( Scope = goal,
      predicate_property(C:Goal, meta_predicate(Meta))
    ->strip_module(C:Goal, I, Head),
      arg(N, Meta, Spec),
      \+ ( integer(Spec),
           Spec >= 0
         ),
      arg(N, Head, Term),
      ( Spec = (:)
      ->strip_module(I:Term, M, Arg)
      ; M = I,
        Head = Arg
      )
    ; strip_module(C:Goal, M, Arg)
    ),
    sub_term(Sub, Arg),
    nonvar(Sub),
    '$termhide'(Sub, T),
    M \= T,
    print_message(error, format("Explicit usage of hidden term ~w not allowed", [T:Sub])).

term_expansion((:- Decl), Clauses) :-
    !,
    term_expansion_decl(Decl, Clauses).
% Next line reexports this module in cascade, to prevent usage of the term
% beyond a second level dependency:
term_expansion(end_of_file, [(:- reexport(library(termhide))), end_of_file]) :-
    \+ prolog_load_context(module, termhide),
    !.
term_expansion(Head :- _, _) :-
    !,
    scan(term, Head),
    fail.
term_expansion(Head --> _, _) :-
    !,
    scan(term, Head),
    fail.
term_expansion(Head, _) :-
    scan(term, Head),
    fail.

goal_expansion(Goal, _) :-
    scan(goal, Goal),
    fail.
