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

:- module(check_undefined, []).

% A wrapper from library(check)
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(checker)).
:- use_module(library(clambda)).
:- use_module(library(infer_alias)).
:- use_module(library(normalize_pi)).
:- use_module(library(codewalk)).
:- use_module(library(location_utils)).
:- use_module(library(from_utils)).
:- use_module(library(referenced_by)).
:- use_module(library(extra_location)).
:- use_module(library(group_pairs_or_sort)).

:- multifile
    prolog:message//1.

:- dynamic
    undef/3.

checker:check(undefined, Results, Options) :-
    check_undefined(Options, Results).

check_undefined(Options, Results) :-
    walk_code([source(true),
               trace_reference(-),
               undefined(trace),
               on_trace(collect_undef)|Options]),
    findall(File-(AL-(PI-(Loc/['~w'-[CI]]))),
            ( retract(undef(PI, CI, From)),
              find_alternatives(PI, AL),
              from_location(From, Loc),
              from_to_file(From, File)
            ), Pairs),
    group_pairs_or_sort(Pairs, Grouped),
    findall(error-(File-(Decl-(PI-LocCI))),
            ( member(File-ALPILocCIList, Grouped),
              member(AL-PILocCIList, ALPILocCIList),
              maplist(\ ((_:F/A)-_)^(F/A)^true, PILocCIList, PIL),
              maplist(alternative_decl(PIL), AL, Decl),
              member(PI-LocCIList, PILocCIList),
              member(LocCI, LocCIList)
            ), Results).

alternative_decl(PIL, A-EM/EL, Decl/FL) :-
    ( EL = []
    ->Decl = use_module(A)
    ; length(EL,  EN),
      length(PIL, IN),
      ( EN < IN
      ->Decl = use_module(A, except(EL))
      ; Decl = use_module(A, PIL)
      )
    ),
    subtract(PIL, EM, FL).

hide_undef(M:H) :- hide_undef(H, M).

find_alternatives(M:F/A, AL) :-
    functor(H, F, A),
    findall(AA-EL, ( current_predicate(AM:F/A),
                     AM \= M,
                     \+ predicate_property(AM:H, imported_from(_)),
                     ( module_property(AM, file(AF))
                     ->
                       \+ ( member(Decl, [multifile, discontiguous, dynamic]),
                            loc_declaration(H, AM, Decl, From),
                            from_to_file(From, File),
                            File \= AF
                          ),
                       ( library_alias(AF, AA)
                       ->true
                       ; AA = AF
                       )
                     ; AA=AM
                     ),
                     exclude_list(M, AM, EL)
                   ), AU),
    sort(AU, AL).

exclude_list(M, AM, ML/EL) :-
    module_property(AM, exports(MU)),
    sort(MU, ML),
    findall(F/A,
            ( member(F/A, ML),
              functor(H, F, A),
              predicate_property(M:H, defined),
              \+ predicate_property(M:H, imported_from(AM))
            ), EU),
    sort(EU, EL).

% Hook to hide undef messages:
:- multifile hide_undef/2.
hide_undef(asr_head_prop(_, _, _, _, _, _, _, _), assertions).

:- public collect_undef/3.
collect_undef(MCall, Caller, From) :-
    \+ hide_undef(MCall),
    normalize_pi(MCall, PI),
    update_fact_from(undef(PI, Caller), From).

prolog:message(acheck(undefined)) -->
    ['Undefined Predicates',nl,
     '--------------------',nl],
    prolog:message(check(undefined_procedures, [])).
prolog:message(acheck(undefined, File-ALPILocCIList)) -->
    [ 'Undefined predicates in ~w:'-[File], nl],
    foldl(show_alternatives, ALPILocCIList),
    { pairs_values(ALPILocCIList, PILocCIList) },
    foldl(foldl(show_undefined), PILocCIList).

show_alternatives(AL-_) -->
    ( {AL = []}
    ->[]
    ; ['  Can be fixed by adding '],
      ( {AL = [_]}
      ->{Spc=''},
        []
      ; {Spc='\t'},
        ['one of these:', nl]
      ),
      foldl(show_alternative(Spc), AL)
    ).

show_alternative(Spc, Decl/FL) -->
    ['~a:- ~q.'-[Spc, Decl]],
    ( {FL = []}
    ->[]
    ; [' % add exports: ~q'-[FL]]
    ),
    [nl].

show_undefined(PI-LocCIList) -->
    [ '    ~w undefined, '-[PI], 'referenced by', nl],
    referenced_by(LocCIList).
