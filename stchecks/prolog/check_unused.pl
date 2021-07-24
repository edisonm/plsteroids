/*  Part of Static Checks

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/xtools
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

:- module(check_unused, []).

/*

  This analyzer is based on the Mark-and-Sweep Algorithm:
  http://www.brpreiss.com/books/opus5/html/page424.html

*/

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(checker)).
:- use_module(library(clambda)).
:- use_module(library(commited_retract)).
:- use_module(library(qualify_meta_goal)).
:- use_module(library(checkable_predicate)).
:- use_module(library(current_defined_predicate)).
:- use_module(library(codewalk)).
:- use_module(library(extra_location)).
:- use_module(library(from_utils)).
:- use_module(library(is_entry_point)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(ungroup_keys_values)).
:- use_module(library(compact_goal)).
:- use_module(library(condconc)).

:- multifile
    prolog:message//1.

:- dynamic
    calls_to_initialization/2,
    calls_to_assertion/4,
    calls_to_declaration/2,
    calls_to_clause/3,
    calls_to_predid/4,
    marked_assertion/2,
    marked_predid/2,
    marked_clause/1,
    marked_initialization/0,
    marked_declaration/0,
    marked_exported/2,
    edge/5.

:- public collect_unused/4.
collect_unused(M, MGoal, Caller, From) :-
    record_location_meta(MGoal, M, From, all_call_refs, cu_caller_hook(Caller)).

checker:check(unused, Result, Options) :-
    check_unused(Options, Result).

check_unused(Options1, Pairs) :-
    foldl(select_option_default,
          [method(Method1)-clause],
          Options1, Options2),
    ( \+ memberchk(Method1, [prolog, clause]) % only these methods are supported
    ->Method = clause,
      print_message(
          warning,
          format("Method `~w' not supported yet, using `~w' instead",
                 [Method1, Method]))
    ; Method = Method1
    ),
    ignore(option(module(M), Options1)),
    merge_options(Options2,
                  [source(false), % False, otherwise this will not work
                   method(Method),
                   on_trace(collect_unused(M))
                  ], Options),
    option_module_files(Options, MFileD),
    option_files([module_files(MFileD)], FileD),
    walk_code([module_files(MFileD)|Options]),
    option(concurrent(Concurrent), Options, true),
    mark(Concurrent),
    sweep(FileD, Pairs),
    cleanup_unused.

cleanup_unused :-
    retractall(calls_to_initialization(_, _)),
    retractall(calls_to_assertion(_, _, _, _)),
    retractall(calls_to_declaration(_, _)),
    retractall(calls_to_clause(_, _, _)),
    retractall(calls_to_predid(_, _, _, _)),
    retractall(marked_clause(_)),
    retractall(marked_assertion(_, _)),
    retractall(marked_predid(_, _)),
    retractall(marked_initialization),
    retractall(marked_declaration),
    retractall(marked_exported(_, _)),
    retractall(edge(_, _, _, _, _)).

marked('<assertion>'(M:H)) :- marked_assertion(H, M).
marked(M:H)                :- marked_predid(H, M).
marked(clause(Ref))        :- marked_clause(Ref).
marked('<initialization>') :- marked_initialization.
marked('<declaration>')    :- marked_declaration.
marked('<exported>'(M:H))  :- marked_exported(H, M).

record_marked('<assertion>'(M:H)) :- assertz(marked_assertion(H, M)).
record_marked(M:H)                :- assertz(marked_predid(H, M)).
record_marked(clause(Ref))        :- assertz(marked_clause(Ref)).
record_marked('<initialization>') :- assertz(marked_initialization).
record_marked('<declaration>'   ) :- assertz(marked_declaration).
record_marked('<exported>'(M:H) ) :- assertz(marked_exported(H, M)).

is_entry_caller('<assertion>'(M:H)) :- entry_caller(M, H).
is_entry_caller('<initialization>').
is_entry_caller('<declaration>'   ).
is_entry_caller(M:H) :- entry_caller(M, H).
is_entry_caller(clause(Ref)) :-
    match_head_clause(M:H, Ref), % Unify head
    entry_caller(M, H).

entry_caller(M, H) :-
    ( is_entry_point(H, M) -> true
    ; loc_declaration(H, M, goal, _)
    ).

entry_point(Caller) :-
    calls_to(Caller, _, _),
    is_entry_caller(Caller).

mark(Concurrent) :-
    cond_forall(Concurrent, entry_point(Caller), put_mark(Caller)).

resolve_meta_goal(H, M, G) :-
    ( ( predicate_property(M:H, meta_predicate(Meta))
                                % don`t use inferred_meta_predicate(M:H, Meta)
                                % since actually it is not being used by the
                                % compiler and would lead to incorrect results
      )
    ->qualify_meta_goal(M:H, Meta, G)
    ; G = H
    ).

is_marked(CRef) :-
    copy_term(CRef, Term),
    marked(Term),
    subsumes_term(Term, CRef).

put_mark(CRef) :-
    ( \+ is_marked(CRef)
    ->record_marked(CRef),
      forall(calls_to(CRef, CM, Callee),
             mark_rec(Callee, CM))
    ; true
    ).

mark_rec(H, M) :-
    resolve_meta_goal(H, M, G),
    forall(gen_lit_marks(M:G, CRef), % Widening
           put_mark(CRef)).

%% gen_lit_marks(:Goal, Ref)
%
% Generalization step, we lose precision but avoid loops --EMM
%
% The order of this clauses matters, because we record as marked from the most
% specific to the most generic one !!!
%
% The logic is: a call to a predicate will potentially use:
%
% (1) all the assertions
% (2) the clauses that match, and
% (3) the dynamic calls that match
%
gen_lit_marks(M:G, '<assertion>'(M:P)) :-
    functor(G, F, A),
    functor(P, F, A).          % Build a fresh head without undesirable bindings
gen_lit_marks(MG, clause(Clause)) :-
    match_head_clause(MG, Clause),
    clause_property(Clause, file(_)).    % Static clauses only
gen_lit_marks(G, P) :- copy_term(G, P). % copy term to avoid undesirable bindings

gen_marks(Ref, Ref).
gen_marks('<assertion>'(M:H), clause(Clause)) :-
    match_head_clause(M:H, Clause),
    clause_property(Clause, file(_)).

not_marked(Ref) :-
    \+ ( gen_marks(Ref, Mark),
         marked(Mark)
       ).

not_marked(H, M) :-
    \+ ( gen_lit_marks(M:H, Mark),
         marked(Mark)
       ).

:- meta_predicate match_head_clause(0, -).
match_head_clause(MH, Clause) :-
    catch(clause(MH, _, Clause), _, fail).

current_edge(X, Y) :-
    PI = M:F/A,
    ( X = PI
    ->functor(H, F, A),
      ( CRef = M:H
      ; match_head_clause(M:H, Clause),
        CRef = clause(Clause)
      ),
      freeze(PI2, PI2 \= PI)
    ; ( X = M:F/A-I,
        integer(I)
      ->functor(H, F, A),
        nth_clause(M:H, I, Clause),
        CRef = clause(Clause)
      ; X = M:F/A-asr
      ->functor(H, F, A),
        CRef = '<assertion>'(M:H)
      ; X = M:F/A-dyn
      ->functor(H, F, A),
        CRef = M:H
      )
    ),
    calls_to(CRef, M2, H2),
    functor(H2, F2, A2),
    PI2 = M2:F2/A2,
    ( Y = PI2
    ; ( match_head_clause(M2:H2, YRef),
        nth_clause(_, I2, YRef),
        Y = M2:F2/A2-I2
      ; %% extra_location(H2, M2, dynamic(use, _, _), _),
        Y = M2:F2/A2-dyn
      ),
      Y \= X
    ).

% Note: although is not nice, we are using dynamic predicates to cache partial
% results for performance reasons (edge/2), otherwise the analysis will take 20
% times more --EMM
%
sweep(FileD, Pairs) :-
    findall(node(Node, D, From), unmarked(FileD, Node, D, From), UNodes),
    sort(UNodes, Nodes),
    maplist(get_adjl(Nodes), Nodes, AdjL),
    maplist(add_sort_by(AdjL), AdjL, AdjSG),
    ungroup_keys_values(AdjSG, AdjSL),
    ungroup_keys_values([warning-AdjSL], Pairs).

get_adjl(Nodes, node(X, DX, FX), node(X, DX, LX)-YL) :-
    from_location(FX, LX),
    findall(Y,
            (   current_edge(X, Y),
                memberchk(node(Y, _, _), Nodes)
                *-> true
            ;   Y = []
            ), YU),
    sort(YU, YL).

add_sort_by(AdjL, Node-CalleeL, sort_by(InclN, LoopN, CalleeN)/Node-CalleeL) :-
    Node = node(X, _, _),
    findall(Caller, ( member(Caller-XL, AdjL),
                      member(X, XL)
                    ), CallerL),
    ( partition(\=(Node), CallerL, InclL, LoopL),
      length(InclL, InclN),
      length(LoopL, LoopN)
    ),
    length(CalleeL, CalleeN).

% Due to the nature of this algorithm, its 'declarative' equivalent is by far
% more difficult to understand, maintain and slower, instead it is implemented
% using dynamic facts.
checker:prepare_results(unused, Pairs, Results) :-
    maplist(\ (warning-Value)^Value^true, Pairs, Values),
    sort(Values, Sorted),
    maplist(assert_edge, Sorted),
    compact_results(Compact),
    maplist(\ Result^(warning-Result)^true, Compact, Results).

assert_edge(SortBy/node(X, D, L)-Y) :-
    ( Y = node(NY, _, _)
    ->true
    ; NY = Y
    ),
    assert(edge(SortBy, X, D, L, NY)).

compact_results(Results) :-
    findall(Result, compact_result(_, Result), Results).

compact_result(X, node(SortBy, L, D, X)-ResultL) :-
    repeat,
      ( edge(SortBy, X, D, L, _)
      ->true
      ; !,
        fail
      ),
      findall(Result,
              ( commited_retract(edge(_, X, D, L, Y)),
                Y \= X, % loop
                compact_result(Y, Result)
              ), ResultU),
      sort(ResultU, ResultL).

/*
sweep(Ref, Pairs) :-
    findall(warning-(Loc-(PI/D)), ( unmarked(Ref, PI),
                                    property_location(PI, D, Loc)), Pairs).
*/

semantic_head(H, M, dyn, dynamic(Type, CM, Call), Caller, From) :-
    loc_dynamic(H, M, dynamic(Type, CM, Call), From),
    ( Type = def
    ->Caller = M:H
    ; Type = dec
    ->functor(H, F, A),
      functor(P, F, A),
      Caller = M:P
    ).
semantic_head(H, M, asr, assertion(S, T), '<assertion>'(M:H), From) :-
    assertions:asr_head_prop(_, CM, H, S, T, _, _, From),
    predicate_property(CM:H, implementation_module(M)).
semantic_head(H, M, exp, export, '<exported>'(M:H), From) :-
    loc_declaration(H, M, export, From).

checkable_unused(Ref) :-
    Ref = M:H,
    checkable_predicate(Ref),
    once(( \+ entry_caller(M, H)
         ; predicate_property(Ref, exported),
           \+ predicate_property(Ref, public)
         )).

unmarked(FileD, Node, D, From) :-
    Ref = M:H,
    MPI = M:F/A,
    ( current_defined_predicate(Ref),
      functor(H, F, A),
      checkable_unused(Ref),
      ( not_marked(H, M)
      ->Node = MPI,
        property_from(Ref, D, From),
        check_pred_file(Ref, FileD, From)
      ; ( match_head_clause(M:H, CRef),
          clause_property(CRef, file(_)), % Static clauses only
          From = clause(CRef),
          not_marked(From),
          check_pred_file(Ref, FileD, From),
          nth_clause(M:H, I, CRef),
          D = clause(I)
        ; semantic_head(H, M, I, D, Mark, From),
          not_marked(Mark),
          check_pred_file(Ref, FileD, From)
        ),
        Node = M:F/A-I
      )
    ; semantic_head(H, M, I, D, Mark, From),
      not_marked(Mark),
      functor(H, F, A),
      check_pred_file(Ref, FileD, From),
      \+ current_predicate(_, Ref),
      checkable_unused(Ref),
      Node = M:F/A-I
    ).

check_pred_file(Ref, FileD, From) :-
    \+ hide_unused_from(Ref, From),
    from_to_file(From, File),
    get_dict(File, FileD, _),
    !.

prolog:message(acheck(unused)) -->
    ['Unused Predicates',nl,
     '-----------------',nl,
     'The predicates has been implemented, however they are', nl,
     'never referenced in the code nor exported.  Probably are', nl,
     'dead-code, part of an incomplete implementation, or called', nl,
     'indirectly by some meta predicate without or with incorrect', nl,
     'meta_predicate declaration.  In any case this represents a', nl,
     'bad design and must be fixed, either completing the program',nl,
     'or removing the unreferenced predicates.', nl, nl].
prolog:message(acheck(unused, Node-EdgeLL)) -->
    message_unused_node(Node, ['*', ' ']),
    foldl(foldl(message_unused_rec([' ', ' ', ' ', ' '])), EdgeLL).

message_unused_node(node(sort_by(N, L, _), F, D, PI), Level) -->
    { R is N + L,
      unused_type(R, T)
    },
    /* Uncomment to help debugging:
    ( { Level = ['*'|_],
        N \= 0
      }
    ->( {ARL \= []}
      ->['In ~w ~w, called from ~w: calls to unused ~w already reported'-[T, PI, L, ARL], nl]
      ; ['In ~w ~w: called from ~w'-[T, PI, L], nl]
      )
    ; []
    ),
    */
    message_unused(T, Level, PI, F/D).

message_unused_rec(Level, Node-EdgeL) -->
    message_unused_node(Node, Level),
    foldl(message_unused_rec([' ', ' '|Level]), EdgeL).

message_unused(T, Level, PI, Loc/D) -->
    Level,
    Loc,
    ['~w ~w: ~w'-[T, D, PI], nl].

unused_type(0, 'unreferenced') :- !.
unused_type(_, 'unreachable' ).

% Hook to hide unused messages:
:- multifile
    hide_unused/2,
    hide_unused_from/2.

hide_unused('$exported_op'(_, _, _), _).
hide_unused('$mode'(_, _), _).
hide_unused('$tabled'(_, _), _).
hide_unused('$table_mode'(_, _, _), _).
hide_unused('$table_update'(_, _, _, _), _).
hide_unused('$pldoc'(_, _, _, _), _).
hide_unused(attr_unify_hook(_, _), predopts_analysis).
hide_unused(location(_, _, _), http).
hide_unused(loading(_), shlib).
hide_unused('pce catcher'(_, _), pce_global).
hide_unused(attribute_goals(_, _, _), M) :- unused_mo_clpfd(M).
hide_unused(attr_unify_hook(_, _),    M) :- unused_mo_clpfd(M).
hide_unused(_, plunit).
hide_unused(_, ciao).
hide_unused(Call, _) :-
    functor(Call, Name, _),
    member(Prefix, ['__aux_wrapper_',
                    '__wrap$',
                    '$wrap$'
                   ]),
    atom_concat(Prefix, _, Name).
hide_unused(Call, _) :-
    current_predicate(apply_macros:maplist_expansion/1),
    apply_macros:maplist_expansion(Call).
hide_unused(Call, M) :-
    functor(Call, Func, Arity),
    member(Prefix, [assert_, asserta_, retract_, retractall_]),
    atom_concat(Prefix, Name, Func),
    functor(Generic, Name, Arity),
    persistency:persistent(M, Generic, _).

hide_unused_from(M:H, _) :- hide_unused(H, M).

unused_mo_clpfd(clpfd_original).
unused_mo_clpfd(clpfd_relation).
unused_mo_clpfd(clpfd_gcc_occurred).
unused_mo_clpfd(clpfd_gcc_num).
unused_mo_clpfd(clpfd_gcc_vs).
unused_mo_clpfd(clpfd_gcc_aux).
unused_mo_clpfd(clpfd_aux).

caller_ptr('<initialization>', _, '<initialization>') :- !.
caller_ptr('<assertion>'(AH),  _, '<assertion>'(AH) ) :- !.
caller_ptr('<declaration>',    _, '<declaration>'   ) :- !.
caller_ptr(_,        clause(Ptr), clause(Ptr)       ) :- !.
caller_ptr(M:H,                _, M:H).

cu_caller_hook(Caller, Head, CM, Type, Goal, _, From) :-
    callable(Head),
    nonvar(CM),
    predicate_property(CM:Head, implementation_module(M)),
    ( Type \= lit
    ->compact_goal(Goal, Comp),
      record_location_goal(Head, M, Type, CM, Comp, From)
    ; Caller = '<assertion>'(A:H),
      member(Goal, [foreign_props:fimport(_),
                    foreign_props:fimport(_, _),
                    foreign_props:tgen(_),
                    foreign_props:sgen(_)])
    ->( A \= CM
      ->put_mark('<exported>'(A:H))
      ; put_mark(A:H)
      )
    ; true
    ),
    record_calls_to(Type, Caller, Head, M, From),
    ( M \= CM
    ->put_mark('<exported>'(M:Head))
    ; true
    ).

record_calls_to(Type, Caller, Head, M, From) :-
    ( memberchk(Type, [use, lit])
    ->caller_ptr(Caller, From, Ptr),
      record_calls_to(Ptr, M, Head)
    ; true
    ).

calls_to('<initialization>',   M, Head) :- calls_to_initialization(   Head, M).
calls_to('<assertion>'(AM:AH), M, Head) :- calls_to_assertion(AH, AM, Head, M).
calls_to('<declaration>',      M, Head) :- calls_to_declaration(      Head, M).
calls_to(clause(Ref),          M, Head) :- calls_to_clause(Ref,       Head, M).
calls_to(CM:CH,                M, Head) :- calls_to_predid(CH, CM,    Head, M).

record_calls_to('<initialization>',   M, Head) :- assertz(calls_to_initialization(   Head, M)).
record_calls_to('<assertion>'(AM:AH), M, Head) :- assertz(calls_to_assertion(AH, AM, Head, M)).
record_calls_to('<declaration>',      M, Head) :- assertz(calls_to_declaration(      Head, M)).
record_calls_to(clause(Ref),          M, Head) :- assertz(calls_to_clause(Ref,       Head, M)).
record_calls_to(CM:CH,                M, Head) :- assertz(calls_to_predid(CH, CM,    Head, M)).
