/*  Part of Static Checks

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/stchecks
    Copyright (C): 2022, Process Design Center, Breda, The Netherlands.
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

:- module(deref_modules,
          [ update_depends_of/0,
            module_links/6
          ]).

:- use_module(library(calls_to)).
:- use_module(library(module_uses)).

ref_head('<assertion>'(M:H), M, H).
ref_head(clause(Ref), M, H) :- freeze(Ref, nth_clause(M:H, _, Ref)).
ref_head(M:H, M, H).

pred_calls_to(AH, AM, H, M) :-
    ref_head(Ref, AM, AH),
    calls_to(Ref, M, H).

:- dynamic
    depends_of/6.

update_depends_of :-
    forall(( pred_calls_to(CH, CM, TH, TM),
             functor(CH, CF, CA),
             functor(TH, TF, TA),
             \+ depends_of(CF, CA, CM, TF, TA, TM)
           ),
           assertz(depends_of(CF, CA, CM, TF, TA, TM))),
    ( predicate_property(depends_of(_, _, _, _, _, _), number_of_clauses(N))
    ->update_depends_of_rec(N)
    ; true
    ).

update_depends_of_rec(N1) :-
    forall(( depends_of(CF, CA, CM, IF, IA, IM),
             depends_of(IF, IA, IM, TF, TA, TM),
             \+ depends_of(CF, CA, CM, TF, TA, TM)
           ),
           assertz(depends_of(CF, CA, CM, TF, TA, TM))),
    predicate_property(depends_of(_, _, _, _, _, _), number_of_clauses(N2)),
    ( N1 \= N2
    ->update_depends_of_rec(N2)
    ; true
    ).

%!  module_links(+Module1, +Module2, +Module3, -UPIL, -PIL23, -PIL21) is det.
%
%   Used to help to break the link between Module1, Module2 and Module3 by
%   suggesting how to reorganize the predicates in Module2.  UPIL is a list of
%   predicates in Module2, used in Module1, that depends on Module3, and
%   therefore prevents the independence of such modules.  PIL23 is the list of
%   predicates in Module2 that depends on Module3.  PIL21 is the list of
%   predicates in Module2 that Module1 depends on.  If UPIL is empty, the link
%   between modules can be broken either by moving the predicates in PIL21 to
%   Module1 or the predicates in PIL23 to Module3.

module_links(Module1, Module2, Module3, UPIL, PIL23, PIL21) :-
    module_uses(Module1, Module2, PIL2),
    module_uses(Module2, Module3, PIL3),
    findall(F2/A2,
            ( member(F2/A2, PIL2),
              member(F3/A3, PIL3),
              depends_of(F2, A2, Module2, F3, A3, Module3)
            ), UPIL),
    findall(F2/A2,
            ( member(F3/A3, PIL3),
              depends_of(F2, A2, Module2, F3, A3, Module3)
            ), PIL23),
    findall(F22/A22,
            ( member(F2/A2, PIL2),
              depends_of(F2, A2, Module2, F22, A22, Module2)
            ), PIL21, PIL2).
