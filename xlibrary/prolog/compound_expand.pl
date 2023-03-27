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

:- module(compound_expand,
          [ before/1,
            after/1,
            op(1, fx, '$compound_expand') % Used to detect expansion modules
          ]).

/* <module> Compound expansions

   This module  allows to define  compositional term and goal  expansions, using
   this   module   in   a   module   that   already   defines   the   predicates
   term_expansion/2/4 and goal_expansion/2/4 but don't export them.

   The  composition  of  expansions  is   instrumental  to  grammar  and  syntax
   extensions,  which is  the key  point of  Ciao Prolog,  but not  supported in
   SWI-Prolog. We  do not  need to deal  with all the  complexity that  the Ciao
   package  system  has, so  with  this  helper the  port  of  Ciao Packages  to
   SWI-Prolog  can be  achieved smoothly  and such  modules can  be used  in SWI
   Programs that do not require the Ciao dialect.

   Note:  Use  reexport(library(compound_expand))  in  order for  this  to  work
   efficiently, otherwise you will have to import compound_expand on each of the
   dependent expansions.

@author Edison Mera
*/

:- use_module(library(expansion_module)).
:- use_module(library(lists)).
:- use_module(library(sort)).

:- multifile
    system:term_expansion/4,
    system:goal_expansion/4,
    before/2.

before(_).

after(_).

:- public implemented_pi/1.
:- meta_predicate implemented_pi(:).
implemented_pi(M:F/A) :-
    functor(H, F, A),
    % Can not use current_module/1 at this stage: --EMM
    once(predicate_property(M:H, visible)),
    \+ predicate_property(M:H, imported_from(_)).

expansion_order(>, M1-_, M2-_) :-
    before(M2, M1),
    !.
expansion_order(=, X, X) :- !.
expansion_order(<, _, _).

collect_expansors(M, ExpansorName, ML) :-
    findall(EM-PI,
            ( expansion_module(M, EM),
              phrase(( ( {implemented_pi(EM:ExpansorName/4)}
                       ->[ExpansorName/4]
                       ; []
                       ),
                       ( {implemented_pi(EM:ExpansorName/2)}
                       ->[ExpansorName/2]
                       ; []
                       )
                     ), PI),
              PI \= []
            ), MU),
    predsort(expansion_order, MU, ML).

type_expansors(term, term_expansion, call_term_expansion).
type_expansors(goal, goal_expansion, call_goal_expansion).

do_compound_expansion(M, Type, Term1, Pos1, Term, Pos) :-
    \+ current_prolog_flag(xref, true),
    type_expansors(Type, Expansor, Closure),
    collect_expansors(M, Expansor, ML),
    call('$expand':Closure, ML, Term1, Pos1, Term, Pos), !.

do_compound_expansion(Type, Term1, Pos1, Term, Pos) :-
    '$current_source_module'(M),
    M \= user, % Compound expansions not supported in user module
    do_compound_expansion(M, Type, Term1, Pos1, Term, Pos).

system:goal_expansion(Goal1, Pos1, Goal, Pos) :-
    do_compound_expansion(goal, Goal1, Pos1, Goal, Pos).

:- thread_local lock_compound/0.

compound_term_expansion(Term1, Pos1, Term, Pos) :-
    do_compound_expansion(term, Term1, Pos1, Term2, Pos2),
    \+ (Term1 =@= Term2),
    % continue with other expansions:
    ( setup_call_cleanup(assertz(lock_compound),
                         '$expand':expand_terms(call_term_expansion([system-[term_expansion/4]]), Term2, Pos2, Term, Pos),
                         retractall(lock_compound))
    ->true
    ; Term = Term2,
      Pos  = Pos2
    ).

system:term_expansion(:- before(B), [compound_expand:before(A, B)]) :-
    '$current_source_module'(A).
system:term_expansion(:- after(B), [compound_expand:before(B, A)]) :-
    '$current_source_module'(A).
system:term_expansion(Term1, Pos1, Term, Pos) :-
    \+ lock_compound,
    compound_term_expansion(Term1, Pos1, Term, Pos).
