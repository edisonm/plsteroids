/*  Part of Static Checks

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/stchecks
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

:- module(check_imports, []).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(checker)).
:- use_module(library(clambda)).
:- use_module(library(calls_to)).
:- use_module(library(expansion_module)).
:- use_module(library(codewalk)).
:- use_module(library(extra_location)).
:- use_module(library(from_utils)).
:- use_module(library(option_utils)).
:- use_module(library(location_utils)).
:- use_module(library(module_files)).
:- use_module(library(cohesive)).
:- init_expansors.

:- multifile
    prolog:message//1.

prolog:message(acheck(imports)) -->
    ['Unused Imports',nl,
     '--------------',nl,
     'The predicates or modules below has been imported, however they', nl,
     'are never used in the importing module, or they do not implement', nl,
     'new clauses for multifile predicates.  Note that modules that', nl,
     'export operators, or that do not export any predicate are not', nl,
     'reported.', nl,
     'You can silent the warnings by declaring use_module/2 with an',nl,
     'empty import list. If they have desirable side effects and still', nl,
     'needs to be imported, you can refactorize your program so that', nl,
     'such side effects are not required anymore.', nl, nl].
prolog:message(acheck(imports, c(Class, Type, Name)-LocElemL)) -->
    ['~w ~w have unused ~w:'-[Class, Name, Type], nl],
    foldl(unused_import, LocElemL).

unused_import(Loc/Elem) -->
    Loc,
    ['unused ~w'-[Elem], nl].

:- dynamic
    used_import/1,
    used_usemod/2.

checker:check(imports, Result, Options) :-
    check_imports(Options, Result).

check_imports(Options, Pairs) :-
    option_module_files(Options, MFileD),
    walk_code([source(false),
               module_files(MFileD),
               concurrent(false),
               on_head(head_used_usemod),
               on_trace(collect_imports_wc)|Options]),
    collect_imports(MFileD, Pairs, Tail),
    collect_usemods(MFileD, Tail, []),
    cleanup_imports.

:- public head_used_usemod/2.

head_used_usemod(Head, From) :-
    record_multifile_deps(Head, From),
    fail.
head_used_usemod(Head, From) :-
    record_head_deps(Head, From),
    fail.

mark_used_usemod(CM, M) :-
    ( M \= CM,
      \+ used_usemod(CM, M)
    ->assertz(used_usemod(CM, M))
    ; true
    ).

record_multifile_deps(Head, From) :-
    caller_impl_module(Head, M),
    from_to_file(From, File),
    module_property(CM, file(File)),
    mark_used_usemod(CM, M).

record_head_deps(Head, From) :-
    calls_to_hook(Head, From, CM, Goal),
    predicate_property(CM:Goal, implementation_module(IM)),
    mark_used_usemod(CM, IM).

:- public collect_imports_wc/3.
:- meta_predicate collect_imports_wc(0,+,+).

collect_imports_wc(M:Goal, Caller, From) :-
    record_location_meta(M:Goal, _, From, all_call_refs, mark_import),
    nonvar(Caller),
    from_to_module(From, CM),
    ( caller_impl_module(Caller, CI),
      atom(CI)
    ->mark_used_usemod(CM, CI)
    ; true
    ),
    mark_used_usemod(CM, M),
    predicate_property(M:Goal, implementation_module(IM)),
    ( '$cohesive'(Goal, IM)
    ->functor(Goal, F, A),
      cohesive:( aux_cohesive_pred(Goal, CohM, Scope, HExt),
                 aux_cohesive_module(M, F, A, CohM, CheckCohM)
               ),
      freeze_cohesive_module_rt(Goal, M, IM, CohM, Scope, CheckCohM),
      forall(clause(IM:HExt, _, _),
             mark_used_usemod(CM, CohM))
    ; true
    ).

caller_impl_module(M:_, M).
caller_impl_module('<assertion>'(M:_), M).

collect_imports(MFileD, Pairs, Tail) :-
    findall(warning-(c(use_module, import, U)-(Loc/(F/A))),
            current_unused_import(MFileD, U, Loc, F, A),
            Pairs, Tail).

current_unused_import(MFileD, U, Loc, F, A) :-
    get_dict(M, MFileD, FileD),
    clause(loc_declaration(Head, M, import(U), From), _, CRef),
    \+ used_import(CRef),
    M \= user,
    from_to_file(From, File),
    get_dict(File, FileD, _),
    \+ memberchk(Head, [term_expansion(_,_),
                        term_expansion(_,_,_,_),
                        goal_expansion(_,_),
                        goal_expansion(_,_,_,_),
                        except(_)
                       ]),
    \+ loc_declaration(Head, M, goal, _),
    module_property(M, class(Class)),
    memberchk(Class, [user]),
    functor(Head, F, A),
    from_location(From, Loc).

:- multifile ignore_import/2.

ignore_import(_, rtchecks_rt).
ignore_import(_, IM) :-
    is_expansion_module(IM),
    % assume discontiguous declaration for goal expansion means it was generated
    % automatically and therefore, check of imports should not be ignored --EMM
    \+ predicate_property(IM:goal_expansion(_, _), discontiguous),
    \+ predicate_property(IM:goal_expansion(_, _, _, _), discontiguous).
ignore_import(_, IM) :-
    '$def_modules'([goal_expansion/4,
                    goal_expansion/2,
                    term_expansion/4,
                    term_expansion/2],
                   MList),
    member(M-PIL, MList),
    member(F/A, PIL),
    functor(H, F, A),
    clause(M:H, _, Ref),
    clause_property(Ref, file(File)),
    module_file(IM, File).
ignore_import(M, IM) :-
    http_dispatch:handler(_, M:H2, _, _),
    functor(H2, Name, A2),
    succ(A2, A),
    functor(H, Name, A),
    predicate_property(M:H, implementation_module(IM)).

collect_usemods(MFileD, Pairs, Tail) :-
    findall(warning-(c(module, use_module, M)-(Loc/U)),
            ( current_unused_use_module(MFileD, U, M, From),
              from_location(From, Loc)
            ), Pairs, Tail).

current_unused_use_module(MFileD, UE, M, From) :-
    get_dict(M, MFileD, FileD),
    M \= user,
    module_property(M, class(Class)),
    memberchk(Class, [user]),
    ( UE = use_module(U),
      loc_declaration(U, M, use_module, From),
      ExL = []
    ; UE = use_module(U, except(ExL)),
      loc_declaration(UE, M, use_module_2, From)
    ),
    from_to_file(From, File),
    get_dict(File, FileD, _),
    \+ findall(I, source_file_property(File, included_in(I, _)),
               [_, _|_]),
    absolute_file_name(U, UFile, [file_type(prolog), access(exist),
                                  file_errors(fail)]),
    module_property(UM, file(UFile)),
    \+ ignore_import(M, UM),
    module_property(UM, exports(EL)),
    EL \= [],
    subtract(EL, ExL, PIL),
    \+ ( module_property(UM, exported_operators(OL1)),
         % ignore kludgy compound_expand operator:
         subtract(OL1, [op(1, fx, '$compound_expand')], OL),
         OL \= []
       ),
    \+ ( MHead = UM:Head,
         ( member(F/A, PIL),
           functor(Head, F, A),
           predicate_property(MHead, implementation_module(IM)),
           % Ignore if reexported from compound_expand:
           IM \= compound_expand,
           once(( used_usemod(M, IM)
                ; loc_declaration(Head, M, goal, _)
                ))
         )
       ).

mark_import(Head, CM, _, _, _, _) :-
    nonvar(CM),
    callable(Head),
    predicate_property(CM:Head, implementation_module(M)),
    mark_import(Head, M, CM).

mark_import(Head, M, CM) :-
    with_mutex(used_import,
               forall(( clause(loc_declaration(Head, CM, import(_), _), _, CRef),
                        \+ used_import(CRef)),
                      assertz(used_import(CRef)))),
    with_mutex(used_usemod,
               ( M \= CM,
                 \+used_usemod(CM, M)
               ->assertz(used_usemod(CM, M))
               ; true
               )).

cleanup_imports :-
    retractall(used_import(_)),
    retractall(used_usemod(_, _)).
