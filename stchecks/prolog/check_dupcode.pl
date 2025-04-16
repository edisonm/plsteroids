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

:- module(check_dupcode, []).

:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(pairs)).
:- use_module(library(checker)).
:- use_module(library(check), []).
:- use_module(library(apply_macros), []).
:- use_module(library(assertions)).
:- use_module(library(clambda)).
:- use_module(library(extend_args)).
:- use_module(library(extra_location)).
:- use_module(library(from_utils)).
:- use_module(library(group_pairs_or_sort)).
:- use_module(library(location_utils)).
:- use_module(library(option_utils)).
:- use_module(library(ungroup_keys_values)).
:- init_expansors.

:- multifile
    prolog:message//1,
    ignore_dupcode/3.

% Note: the order of clauses is important, to allow remove redundant information,
% that is, 'predicate' implies 'clause' implies 'name' duplication.
%
% duptype(meta_predicate).
duptype(declaration).
duptype(predicate).
duptype(clause).
duptype(name).

% Use the same group key to allow filtering of redundant messages.
%
element_group(declaration, _-MTE, G) :-
    ( MTE = meta_predicate(M:H)
    ->functor(H, F, A),
      G=meta_predicate(M:F/A)
    ; G = MTE
    ).
element_group(predicate,  _:_:_:F/A,   F/A).
element_group(clause,         _:F/A-_, F/A).
element_group(name,           _:F/A,   F/A).

ignore_dupcode(H, _, _) :-
    functor(H, Name, _),
    member(Prefix, ['__aux_', '$']),
    atom_concat(Prefix, _, Name).
ignore_dupcode(H, _, _) :-
    current_module(apply_macros),
    apply_macros:maplist_expansion(H).
ignore_dupcode(_,                             refactor, name).
ignore_dupcode(_,                        i18n_refactor, name).
ignore_dupcode(term_expansion(_, _),            _,      name).
ignore_dupcode(term_expansion(_, _, _, _),      _,      name).
ignore_dupcode(goal_expansion(_, _),            _,      name).
ignore_dupcode(goal_expansion(_, _, _, _),      _,      name).
ignore_dupcode('$exported_op'(_, _, _),         _,      _).
ignore_dupcode('$mode'(_, _),                   _,      _).
ignore_dupcode('$pred_option'(_, _, _, _),      system, _).
ignore_dupcode('$included'(_, _, _, _),         system, _).
ignore_dupcode('$load_context_module'(_, _, _), system, _).
ignore_dupcode(_,                               prolog, declaration(_)).
ignore_dupcode(_,                               user,   declaration(use_module)).
ignore_dupcode(_,                               user,   declaration(dynamic)).
ignore_dupcode(_,                               _,      declaration(dynamic(_, _, _))).

checker:check(dupcode, Result, Options) :-
    option_module_files(Options, MFileD),
    check_dupcode(MFileD, Result).

:- meta_predicate
    duptype_elem(+, 0, +, -, -).

%!  duptype_elem(+DupType, :Head, :FileChk, -DupId, -Elem) is multi
%
%   For a given Element of the language, returns a duplication key and an
%   associated value
%
duptype_elem(name, M:H, FileD, F/A, M:F/A) :-
    predicate_property(M:H, file(File)),
    get_dict(File, FileD, _),
    functor(H, F, A).
% Note: we wrap the DupId with hash/1 to allow easy identification in saved
% analysis outputs:
duptype_elem(clause, MH, FileD, hash(DupId), M:F/A-Idx) :-
    strip_module(MH, M, H),
    \+ has_dupclauses(H, M),
    nth_clause(MH, Idx, Ref),
    clause(MH, MBody, Ref),
    from_to_file(clause(Ref), File),
    get_dict(File, FileD, _),
    functor(H, F, A),
    strip_module(MBody, _C, Body),
    copy_term_nat((H :- Body), Term),
    variant_sha1(Term, DupId).
duptype_elem(predicate, MH, FileD, hash(DupId), File:Line:M:F/A) :-
    predicate_property(MH, file(File)),
    get_dict(File, FileD, _),
    strip_module(MH, M, H),
    findall((H :- B),
            ( clause(MH, MB),
              strip_module(MB, _, B)
            ), ClauseL),
    findall(File:Line,
            once(( From = clause(Ref),
                   clause(MH, _, Ref),
                   from_to_file(From, File),
                   from_to_line(From, Line)
                 )), [File:Line]),
    copy_term_nat(ClauseL, Term),
    variant_sha1(Term, DupId),
    functor(H, F, A).

duptype_elem_declaration(MFileD, DupId, From-MTE) :-
    loc_declaration(H, M, T, From),
    \+ ignore_dupcode(H, M, declaration(T)),
    get_dict(M, MFileD, FileD),
    from_to_file(From, File),
    get_dict(File, FileD, _),
    \+ memberchk(T, [goal, assertion(_,_)]),
    once(dtype_dupid_elem(T, T, From, H, M, DupId, Elem)),
    extend_args(M:T, [Elem], MTE).

dup_if_same_file(use_module).
dup_if_same_file(consult).
dup_if_same_file(multifile).
dup_if_same_file(discontiguous).

dtype_dupid_elem(meta_predicate, T, _, H, M, T-M:F/A, H) :- functor(H, F, A).
dtype_dupid_elem(T, T, F, H, M, T-File:M:H, H) :-
    dup_if_same_file(T),  % Ignore duplicates from different files
    from_to_file(F, File).
% dtype_dupid_elem(use_module_2,   T, H, M, T-M:H,  T-M:H).
dtype_dupid_elem(T,              T, _, H, M, T-M:PI, G) :-
    ( H =.. [_|Vars1],
      term_variables(H, Vars2),
      Vars1==Vars2
    ->functor(H, F, A),
      PI=F/A,
      G =F/A
    ; PI=H,
      G =H
    ).

ignore_dupgroup((DupType-_)-ElemL) :-
    \+ consider_dupgroup(DupType, ElemL).

consider_dupgroup(DupType, CIL) :-
    append(_, [CI|PIL2], CIL),
    element_head(DupType, CI, MH1),
    consider_dupgroup_1(DupType, MH1),
    member(CI2, PIL2),
    element_head(DupType, CI2, MH2),
    consider_dupgroup_2(DupType, MH1, MH2).

consider_dupgroup_1(predicate, MH) :- \+ predicate_property(MH, public).
consider_dupgroup_1(clause,     _).

consider_dupgroup_2(predicate, _, _).
consider_dupgroup_2(clause, M:_, M:_).

has_dupclauses(H, M) :-
    prop_asr(head, M:H, _, Asr),
    prop_asr(glob, plprops:dupclauses(_), _, Asr).

element_head(predicate, _:_:M:F/A,   M:H) :- functor(H, F, A).
element_head(clause,        M:F/A-_, M:H) :- functor(H, F, A).

curr_duptype_elem(MFileD, DupType, DupId, Elem) :-
    get_dict(M, MFileD, FileD),
    current_predicate(M:F/A),
    functor(H, F, A),
    \+ predicate_property(M:H, imported_from(_)),
    duptype(DupType),
    \+ ignore_dupcode(H, M, DupType),
    duptype_elem(DupType, M:H, FileD, DupId, Elem).
curr_duptype_elem(MFileD, declaration, DupId, Elem) :-
    duptype_elem_declaration(MFileD, DupId, Elem).

check_dupcode(MFileD, Result) :-
    findall((DupType-DupId)-Elem,
            curr_duptype_elem(MFileD, DupType, DupId, Elem), PU),
    sort(PU, PL),
    group_pairs_by_key(PL, GL),
    partition(\ (_-[_])^true, GL, _, GD), % Consider duplicates
    findall(G, ( member(G, GD),
                 \+ ignore_dupgroup(G)
               ), Groups),
    ungroup_keys_values(Groups, Pairs),
    clean_redundants(Pairs, CPairs),
    maplist(add_location, CPairs, Result).

pair_group(Pair, GKey-(DupType-(DupId/Elem))) :-
    Pair = (DupType-DupId)-Elem,
    element_group(DupType, Elem, GKey).

clean_redundants(Pairs, CPairs) :-
    maplist(pair_group, Pairs, GPairs),
    sort(GPairs, GSorted),
    group_pairs_or_sort(GSorted, Groups),
    maplist(clean_redundant_group, Groups, CGroups),
    ungroup_keys_values(CGroups, CPairs).

clean_redundant_group(GKey-Group, (DupType/GKey)-List) :-
    duptype(DupType),
    memberchk(DupType-List, Group), !.

elem_property(name,           PI,        PI,        T, T).
elem_property(clause,         M:F/A-Idx, (M:H)/Idx, T, T) :- functor(H, F, A).

elem_location(declaration, From-_, declaration, Loc) :- !,
    from_location(From, Loc).
elem_location(predicate, File:Line:_:_/_, predicate, Loc) :-
    !,
    from_location(file(File, Line, _, _), Loc).
elem_location(DupType, Elem, D, Loc) :-
    elem_property(DupType, Elem, Prop, T, D),
    property_location(Prop, T, Loc).

add_location(DupType/GKey-DupId/Elem,
             warning-(DupType/GKey-(DupId-(LocDL/Elem)))) :-
    findall(Loc/D, (elem_location(DupType, Elem, D, Loc), D \= goal), LocDU),
    sort(LocDU, LocDL).

prolog:message(acheck(dupcode)) -->
    ['Duplicated Code',nl,
     '---------------',nl,
     'The elements below would has been implemented in different modules,', nl,
     'but are duplicates.  Would be a symptom of duplicated functionality.', nl,
     'In the case of predicate names, at least one has been exported,', nl,
     'making difficult to import it in other modules without clash risk.', nl,
     'This can be fixed by merging the duplicated code, or by refactoring', nl,
     'one of the duplicated to avoid this warning. Note that predicates', nl,
     'declared as public are ignored by this analysis.', nl, nl].
prolog:message(acheck(dupcode, (DupType/GKey)-LocDL)) -->
    ['~w ~w is duplicated:'-[DupType, GKey], nl],
    foldl(message_duplicated, LocDL).

message_duplicated(_-[LocD|LocDL]) -->
    message_duplicated('* ', LocD),
    foldl(message_duplicated('  '), LocDL).

message_duplicated(Pre, LocDL/Elem) -->
    foldl(message_duplicated(Pre, Elem), LocDL).

message_duplicated(Pre, Elem, Loc/D) -->
    [Pre], Loc, ['duplicated '],
    message_elem(D, Elem),
    [nl].

message_elem(declaration, _-Elem) --> !, [':- ~w.'-[Elem]].
message_elem(predicate,   _:_:M:F/A) --> !, ['predicate ~w'-[M:F/A]].
message_elem(Type, Elem) --> ['~w ~w'-[Type, Elem]].
