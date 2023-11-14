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
            init_expansors/0,
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

   Notes:

   - Use   reexport(library(compound_expand))  in   order  for   this  to   work
     efficiently, otherwise you  will have to import compound_expand  on each of
     the dependent expansions,  but also you should avoid to  import this in the
     user module.

   - Expansions are not applied to the module where they are implemented, but to
     the modules that import them.  This  is a bit different from how expansions
     work in SWI-Prolog, but it has a more clear behavior.

@author Edison Mera
*/

:- use_module(library(def_modules), []).

% The most  efficient way  to implement  the compound  expansions library  is to
% redefine the  predicate '$def_modules'/2, which  is only called  in expand.pl,
% but for some  reason I don't know why  we can not redefine it,  so instead the
% next lines are in place:

% :- redefine_system_predicate('$def_modules'(_,_)). % This does not work
:- abolish('$expand':'$def_modules'/2),
   use_module('$expand':library(def_modules), ['$def_modules'/2]).

:- use_module(library(expansion_module)).
:- use_module(library(partsort)).
:- use_module(library(lists)).
:- use_module(library(apply)).
:- use_module(library(option)).

:- multifile
    system:term_expansion/4,
    system:goal_expansion/4,
    before/2.

%!  before(+Module) is det.
%
%   Declaration to say that the expansion provided by this Module must happen
%   before the expansion in Module

before(_).

%!  after(+Module) is det.
%
%   Declaration to say that the expansion provided by this Module must happen
%   after the expansion in Module

after(_).

:- public implemented_pi/1.
:- meta_predicate implemented_pi(:).
implemented_pi(M:F/A) :-
    functor(H, F, A),
    % Can not use current_module/1 at this stage: --EMM
    once(predicate_property(M:H, visible)),
    \+ predicate_property(M:H, imported_from(_)).

expansion_order(>, M1-_, M2-_) :-
    expansion_order_gt(M1, M2),
    !.
expansion_order(=, X, X) :- !.

% Control the expansion orders via reexport, i.e., first the transformations in
% the current library and later the transformation in the reexported one.
expansion_order_gt(M1, M2) :-
    before(M2, M1), % let programmers decide
    !.
expansion_order_gt(M1, M2) :-
    module_property(M1, file(File)),
    current_op(1, fx, M1:'$compound_expand'),
    '$load_context_module'(File, M2, Options),
    option(reexport(true), Options),
    !.
expansion_order_gt(M, M2) :-
    '$load_context_module'(File1, M2, Options),
    option(reexport(true), Options),
    module_property(M1, file(File1)),
    current_op(1, fx, M1:'$compound_expand'),
    expansion_order_gt(M, M1),
    !.

collect_expansor(EM, ExpansorName) -->
    ( {implemented_pi(EM:ExpansorName)}
    ->[ExpansorName]
    ; []
    ).

collect_expansors(ExpansorNameL, M, ML) :-
    findall(EM-PIL,
            ( expansion_module(M, EM),
              foldl(collect_expansor(EM), ExpansorNameL, PIL, []),
              PIL \= []
            ), MU),
    partsort(expansion_order, MU, MO),
    system:'$def_modules'(M:ExpansorNameL, MT),
    append(MO, MT, ML).

%!  init_expansors is det.
%
%   Declaration to say that the compound expansion definition has finish and now
%   the expansions can begin.  This is required to speed up the compilation,
%   since the predicate collect_expansors/3 is expensive but its solution
%   doesn't change once no more expansors are added.
%

init_expansors.

no_more_expansions_after_init(Source) :-
    member(Expansors,
           [[term_expansion/4, term_expansion/2],
            [goal_expansion/4, goal_expansion/2]]),
    collect_expansors(Expansors, Source, TN1),
    system:'$def_modules'(Source:Expansors, DN),
    subtract(TN1, DN, TN),
    ( '$defined_predicate'(Source:'$module_expansors'(_, _))
    ->Source:'$module_expansors'(Expansors, TL1),
      subtract(TL1, DN, TL),
      TL \= TN,
      subtract(TN, TL, EL),
      print_message(warning, format("More expansors added after :- init_expansors declaration: ~w", [EL]))
    ; TN \= []
    ->print_message(warning, format("Missing :- init_expansors declaration, but expansors present: ~w", [TN]))
    ).
system:term_expansion(end_of_file, _) :-
    '$current_source_module'(Source),
    module_property(Source, file(File)),
    prolog_load_context(source, File),
    no_more_expansions_after_init(Source),
    fail.
/* NOTE: this is commented out to let expansions in module qualified literals
 * work, since you need to keep the expansions available --EMM

stop_expansors :-
    '$current_source_module'(Source),
    abolish(Source:'$module_expansors'/2).

system:term_expansion(end_of_file, _) :-
    '$current_source_module'(Source),
    module_property(Source, file(File)),
    prolog_load_context(source, File),
    stop_expansors,
    fail.
*/
system:term_expansion(:- before(B), compound_expand:before(A, B)) :-
    '$current_source_module'(A).
system:term_expansion(:- after( B), compound_expand:before(B, A)) :-
    '$current_source_module'(A).
system:term_expansion((:- init_expansors), []) :-
    '$current_source_module'(Source),
    dynamic(Source:'$module_expansors'/2),
    public(Source:'$module_expansors'/2),
    retractall(Source:'$module_expansors'(_, _)),
    forall(member(Expansors,
                  [[term_expansion/4, term_expansion/2],
                   [goal_expansion/4, goal_expansion/2]]),
           ( collect_expansors(Expansors, Source, TL),
             assertz(Source:'$module_expansors'(Expansors, TL))
           )).
