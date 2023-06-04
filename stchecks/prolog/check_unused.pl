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

:- module(check_unused, []).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(checker)).
:- use_module(library(clambda)).
:- use_module(library(compilation_module)).
:- use_module(library(commited_retract)).
:- use_module(library(checkable_predicate)).
:- use_module(library(current_defined_predicate)).
:- use_module(library(extra_location)).
:- use_module(library(from_utils)).
:- use_module(library(is_entry_point)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(ungroup_keys_values)).
:- use_module(library(condconc)).
:- use_module(library(calls_to)).
:- use_module(library(mark_preds)).

:- init_expansors.

/** <module> Check unused predicates

  This analyzer is based on the Mark-and-Sweep Algorithm:
  http://www.brpreiss.com/books/opus5/html/page424.html

*/

:- multifile
    prolog:message//1.

:- dynamic
    edge/5.

checker:check(unused, Result, Options) :-
    check_unused(Options, Result),
    cleanup_calls_to,
    cleanup_unused.

check_unused(Options, Pairs) :-
    collect_calls_to(Options, MFileD),
    mark_compile_time_called,
    option_files([module_files(MFileD)], FileD),
    option(concurrent(Concurrent), Options, true),
    mark(Concurrent),
    sweep(FileD, Pairs).

cleanup_unused :-
    cleanup_marked,
    retractall(edge(_, _, _, _, _)).

is_entry_caller('<assertion>'(M:H)) :- entry_caller(M, H).
is_entry_caller('<initialization>').
is_entry_caller('<declaration>'   ).
is_entry_caller('<exported>'(_)   ).
is_entry_caller('<public>'(_)     ).
is_entry_caller(M:H) :-
    entry_caller(M, H).
is_entry_caller(clause(Ref)) :-
    match_head_clause(M:H, Ref), % Unify head
    entry_caller(M, H).

entry_caller(M, H) :-
    ( is_entry_point(H, M)
    ->true
    ; loc_declaration(H, M, goal, _)
    ).

entry_point(Caller) :-
    calls_to(Caller),
    is_entry_caller(Caller).

mark(Concurrent) :-
    cond_forall(Concurrent, entry_point(Caller), put_mark(Caller)).

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
    calls_to(CRef, CM2, H2),
    predicate_property(CM2:H2, implementation_module(M2)),
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
    Head = M:H,
    MPI = M:F/A,
    ( current_defined_predicate(Head),
      functor(H, F, A),
      checkable_unused(Head),
      ( not_marked(H, M)
      ->Node = MPI,
        property_from(Head, D, From),
        check_pred_file(Head, FileD, From)
      ; ( match_head_clause(M:H, CRef),
          clause_property(CRef, file(_)), % Static clauses only
          From = clause(CRef),
          not_marked(From),
          check_pred_file(Head, FileD, From),
          nth_clause(M:H, I, CRef),
          D = clause(I)
        ; semantic_head(H, M, I, D, Mark, From),
          not_marked(Mark),
          check_pred_file(Head, FileD, From)
        ),
        Node = M:F/A-I
      )
    ; semantic_head(H, M, I, D, Mark, From),
      not_marked(Mark),
      functor(H, F, A),
      check_pred_file(Head, FileD, From),
      \+ current_predicate(_, Head),
      checkable_unused(Head),
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
     'The predicates has been implemented, however they are never', nl,
     'referenced in the code nor exported.  Probably are dead-code, part', nl,
     'of an incomplete implementation, or called indirectly by some meta', nl,
     'predicate without or with incorrect meta_predicate declaration.', nl,
     'In any case this represents a bad design and must be fixed, either', nl,
     'completing the program or removing the unreferenced predicates.', nl, nl].
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
    ['~w ~w: ~q'-[T, D, PI], nl].

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
    member(Prefix, ['__aux_',
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
