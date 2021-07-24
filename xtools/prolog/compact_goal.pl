/*  Part of Extended Tools for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(compact_goal, [compact_goal/2]).

:- use_module(library(lists)).

% compact_goal/2 is the opposite of expand_goal/2, and is intended to improve
% display of goals that where expanded by the compiler. It is only approximated
% and should not be used in other context.
%
compact_goal(M:Goal, N:Compact) :- !,
    strip_module(M:Goal, N, Head),
    compact_goal(Head, Compact).
compact_goal(Goal, Compact) :-
    ( member(Pattern-Into,
             [(\+ (G, \+E))           -forall(G, E),
              forall(retract(F), true)-retractall(F)]),
      subsumes_term(Pattern, Goal)
    ->Goal = Pattern,
      Goal2 = Into,
      compact_goal(Goal2, Compact)
    ; Compact = Goal
    ).

prolog:called_by(\+ Goal, system, M, CL) :-
    nonvar(Goal),
    subsumes_term((retract(Fact), \+ true), Goal),
    Goal = (retract(Fact), \+ true),
    ( prolog:called_by(retractall(Fact), system, M, CL)
    ->true
    ; CL = [true]               % Skip walk meta arguments
    ).
