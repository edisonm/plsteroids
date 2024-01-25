/*  Part of Extended Libraries for SWI-Prolog

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xlibrary
    Copyright (C): 2023, Process Design Center, Breda, The Netherlands.
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

:- module(checkct,
          [ call_checkct/4
          ]).

:- use_module(library(lists)).
:- use_module(library(compound_expand)).
:- use_module(library(ontrace)).
:- use_module(library(option)).
:- use_module(library(solution_sequences)).
:- use_module(library(in_module_file)).
:- init_expansors.

:- thread_local
    issue_found/5.

%!  call_checkct(:Call, +File, +Line, Options) is multi.
%
%   Meta-predicate to prevent that Call uses dynamic or multifile predicates.
%   This is important to prevent wrong usage of compile-time optimizations on
%   parts of the program intended to be evaluated at run-time.  It was part of
%   library(neck), but now it is in its own module to allow its usage elsewhere.

:- meta_predicate call_checkct(0,+,+,+).

call_checkct(Call, File, Line, Options) :-
    option(ignore(IgnoreL), Options, []),
    ontrace(Call, handle_port(File, Line, IgnoreL), []).

clause_pc_location(clause_pc(Clause, PC), Loc) :-
    clause_pc_location(Clause, PC, Loc),
    !.
clause_pc_location(Loc, Loc).

term_expansion(end_of_file, _) :-
    in_module_file,
    forall(distinct([File, Line, Issues, PI, Loc],
                    ( retract(issue_found(File, Line, Issues, PI, Loc1)),
                      clause_pc_location(Loc1, Loc)
                    )),
           print_message(
               warning,
               at_location(
                   file(File, Line, -1, _),
                   at_location(
                       Loc,
                       format("~w ~w called at compile time", [Issues, PI]))))),
    fail.

% Note: this is not called by make/0, since it is wrapped by notrace/1, you
% should use make:make_no_trace/0 instead --EMM

handle_port(File, Line, IgnoreL, call, Frame, _, _, Loc, Action) :-
    prolog_frame_attribute(Frame, goal, Goal),
    strip_module(Goal, M, Call),
    functor(Call, F, A),
    PI = M:F/A,
    ( memberchk(PI, IgnoreL)
    ->Action = skip
    ; findall(Issue,
            ( member(Issue, [multifile, dynamic]),
              predicate_property(Goal, Issue)
            ), IssueL),
      IssueL \= [],
      atomic_list_concat(IssueL, ',', Issues),
      retractall(issue_found(File, Line, Issues, PI, Loc)),
      assertz(issue_found(File, Line, Issues, PI, Loc)),
      Action = continue
    ).
