/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
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

:- module(choicepoints,
          [no_choicepoints/2,
           has_choicepoints/2]).

:- meta_predicate
    choicepoints(0, 0, 0 ),
    no_choicepoints(0, 0 ),
    has_choicepoints(0, 0 ).

choicepoints(Goal, Handler1, Handler2) :-
    Solved = solved(no),
    prolog_current_choice(C1),
    Goal,
    prolog_current_choice(C2),
    ( arg(1, Solved, no)
    ->( C2 == C1
      ->Handler1
      ; Handler2
      )
    ; true
    ),
    nb_setarg(1, Solved, yes).

%!  no_choicepoints(:Goal, :Handler)
%   calls Handler(Goal) if Goal has no choicepoints

no_choicepoints(Goal, Handler) :-
    choicepoints(Goal, true, Handler).

%!  has_hoicepoints(:Goal, :Handler)
%   calls Handler(Goal) if Goal has choicepoints

has_choicepoints(Goal, Handler) :-
    choicepoints(Goal, Handler, true).
