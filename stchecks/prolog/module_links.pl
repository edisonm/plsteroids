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

:- module(module_links,
          [ update_depends_of/0,
            module_links/6,
            module_links/4,
            module_pred_links/2,
            module_pred_links/3,
            unlink_loop/4
          ]).

:- use_module(library(calls_to)).

ref_head('<assertion>'(M:H), M, H).
ref_head(M:H, M, H).
ref_head(clause(Ref), M, H) :-
    freeze(Ref, clause(M:H, _, Ref)).

pred_calls_to(AH, AM, H, CM) :-
    ref_head(Ref, AM, AH),
    calls_to(Ref, CM, H).

:- dynamic
    depends_of_db/6.

update_depends_of :-
    update_depends_of_1,
    update_depends_of_cm(_).

update_depends_of_1 :-
    forall(( pred_calls_to(AH, AM, TH, CM),
             predicate_property(CM:TH, implementation_module(TM)),
             \+ depends_of_db(AH, AM, TH, TM, CM, _)
           ),
           ( functor(AH, AF, AA), functor(AP, AF, AA),
             functor(TH, TF, TA), functor(TP, TF, TA),
             assertz(depends_of_db(AP, AM, TP, TM, CM, 1))
           )).

% resolve recursion explicitly for those dependencies inside the same module to
% avoid performance issues: we use tabling, but we also use an index to prevent
% performance problems, otherwise it will try all the possible paths between two
% predicates, which is not needed actually

:- table update_depends_of_cm/1.

update_depends_of_cm(CM) :-
    update_depends_of_cm_rec(CM, 1).

update_depends_of_cm_rec(CM, N1) :-
    succ(N1, N),
    forall(( depends_of_db(AH, AM, IH, IM, CM, N1),
             depends_of_db(IH, IM, TH, TM, CM, 1),
             \+ depends_of_db(AH, AM, TH, TM, CM, _)
           ),
           assertz(depends_of_db(AH, AM, TH, TM, CM, N))),
    ( depends_of_db(_, _, _, _, _, N)
    ->update_depends_of_cm_rec(CM, N)
    ; true
    ).

depends_of_cm(AH, AM, TH, TM, CM) :-
    depends_of_db(AH, AM, TH, TM, CM, _).
depends_of_cm(AH, AM, TH, TM, CM) :-
    depends_of_db(AH, AM, IH, AM, CM, _),
    depends_of_db(IH, AM, TH, TM, CM, _).

:- meta_predicate collect_dependents(1, +, -).

collect_dependents(GetPI2, Module2, PIL21) :-
    findall(PI21,
            ( call(GetPI2, M2:H2),
              ( functor(H2, F21, A21)
              ; depends_of_db(H2, M2, H21, Module2, Module2, _),
                functor(H21, F21, A21)
              ),
              ( M2 = Module2
              ->PI21 = F21/A21
              ; PI21 = M2:F21/A21
              )
            ), PIU21),
    sort(PIU21, PIL21).

%!  module_links(+Module1, +Module2, +Module3, -UPIL, -PIL21, -PIL23) is det.
%
%   Used to help to break the link between Module1, Module2 and Module3 by
%   suggesting how to reorganize the predicates in Module2.  UPIL is a list of
%   predicates in Module2, used in Module1, that depends on Module3, and
%   therefore prevents the independence of such modules.  PIL23 is the list of
%   predicates in Module2 that depends on Module3.  PIL21 is the list of
%   predicates in Module2 that Module1 depends on.  If UPIL is empty, the link
%   between modules can be broken either by moving the predicates in PIL21 to
%   Module1 or the predicates in PIL23 to Module3.

module_links(Module1, Module2, Module3, UPIL, PIL21, PIL23) :-
    module_links(Module1, Module2, Module3, UPIL),
    collect_dependents(module_uses(Module1, Module2), Module2, PIL21),
    collect_dependents(uses_module(Module2, Module3), Module2, PIL23).

module_links(Module1, Module2, Module3, UPIL) :-
    findall(PI, current_module_link(Module1, Module2, Module3, PI), UPIL).

current_module_link(Module1, Module2, Module3, M2:F2/A2) :-
    depends_of_db(_, _, H2, Module2, Module1, _),
    once(depends_of_cm(H2, M2, _, Module3, Module2)),
    functor(H2, F2, A2).

module_pred_links(ModuleL1, PILL) :-
    module_pred_links(ModuleL1, _, PILL).

module_pred_links(ModuleL1, PIL1, PILL) :-
    last(ModuleL1, Last),
    ModuleL1 = [First|ModuleT1],
    append(ModuleT1, [First], ModuleL2),
    module_pred_link(Last, First, PILast),
    foldl(module_pred_link, ModuleL1, ModuleL2, PIL1, PILast, PIFirst),
    foldl(module_pred_link, ModuleL1, ModuleL2, PILL, PIFirst, _).

module_pred_link(Module1, Module2, PIL) :-
    findall(Module2:F2/A2,
            ( depends_of_db(_, _, H2, Module2, Module1, _),
              functor(H2, F2, A2)
            ), PIU),
    sort(PIU, PIL).

module_pred_link(Module1, Module2, PIL1, PIL2) :-
    findall(Module2:F2/A2,
            ( member(M1:F1/A1, PIL1),
              functor(H1, F1, A1),
              depends_of_cm(H1, M1, H2, Module2, Module1),
              functor(H2, F2, A2)
            ), PIU2),
    sort(PIU2, PIL2).

module_pred_link(Module1, Module2, PIL1, PIL1, PIL2) :-
    module_pred_link(Module1, Module2, PIL1, PIL2).

module_uses(Module1, Module2, Module2:H2) :-
    depends_of_db(_, _, H2, Module2, Module1, _).
    
uses_module(Module2, Module3, M2:H2) :-
    depends_of_cm(H2, M2, _, Module3, Module2).

unlinkable_chain(ModuleL1, Module1, Module2, Module3) :-
    last(ModuleL1, Last),
    ModuleL1 = [First|_],
    append([Last|ModuleL1], [First], ModuleL),
    append(_, [Module1, Module2, Module3|_], ModuleL),
    \+ current_module_link(Module1, Module2, Module3, _).

unlink_loop(ModuleL, Module2, PIL21->Module1, PIL23->Module3) :-
    unlinkable_chain(ModuleL, Module1, Module2, Module3),
    collect_dependents(module_uses(Module1, Module2), Module2, PIL21),
    collect_dependents(uses_module(Module2, Module3), Module2, PIL23).
