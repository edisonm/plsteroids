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
          [ update_depends_of/2,
            module_links/6,
            module_links/4,
            unlink_loop/2,
            unlink_loop/4
          ]).

:- use_module(library(calls_to)).
:- use_module(library(module_uses)).

ref_head('<assertion>'(M:H), M, H).
ref_head(M:H, M, H).
ref_head(clause(Ref), M, H) :-
    freeze(Ref, clause(M:H, _, Ref)).

pred_calls_to(AH, AM, H, M) :-
    ref_head(Ref, AM, AH),
    calls_to(Ref, M, H).

:- dynamic
    depends_of_db/4.

update_depends_of(CM, TM) :-
    forall(( pred_calls_to(CH, CM, TH, TM),
             \+ depends_of_db(CH, CM, TH, TM)
           ),
           ( functor(CH, CF, CA), functor(CP, CF, CA),
             functor(TH, TF, TA), functor(TP, TF, TA),
             assertz(depends_of_db(CP, CM, TP, TM))
           )).

% resolve recursion explicitly for those dependencies inside the same module:
update_depends_of_cm(Module) :-
    (   depends_of_db(CH, Module, IH, Module),
        depends_of_db(IH, Module, TH, Module),
        \+ depends_of_db(CH, Module, TH, Module)
    ->  assertz(depends_of_db(CH, Module, TH, Module)),
        update_depends_of_cm(Module)
    ;   true
    ).

depends_of_cm(CH, CM, TH, TM) :-
    depends_of_db(CH, CM, TH, TM).
depends_of_cm(CH, CM, TH, TM) :-
    depends_of_db(CH, CM, IH, CM),
    depends_of_db(IH, CM, TH, TM).

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
    update_depends_of_cm(Module2),
    findall(F2/A2,
            ( module_uses(Module1, Module2, F2, A2),
              functor(H2, F2, A2),
              once(( module_uses(Module2, Module3, F3, A3),
                     functor(H3, F3, A3),
                     depends_of_cm(H2, Module2, H3, Module3)
                   ))
            ), UPIL).

uses_module(Module2, Module3, F2, A2) :-
    module_uses(Module2, Module3, F3, A3),
    functor(H3, F3, A3),
    depends_of_cm(H2, Module2, H3, Module3),
    functor(H2, F2, A2).

collect_dependents(GetPI2, Module2, PIL21) :-
    findall(F21/A21,
            ( call(GetPI2, F2, A2),
              ( F21 = F2,
                A21 = A2
              ; functor(H2, F2, A2),
                depends_of_db(H2, Module2, H21, Module2),
                functor(H21, F21, A21)
              )
            ), PIU21),
    sort(PIU21, PIL21).

unlink_loop(ModuleL1, Module2) :-
    last(ModuleL1, Last),
    ModuleL1 = [First|_],
    append([Last|ModuleL1], [First], ModuleL),
    append(_, [Module1, Module2, Module3|_], ModuleL),
    module_links(Module1, Module2, Module3, []).

unlink_loop(ModuleL1, Module2, PIL21->Module1, PIL23->Module3) :-
    last(ModuleL1, Last),
    ModuleL1 = [First|_],
    append([Last|ModuleL1], [First], ModuleL),
    append(_, [Module1, Module2, Module3|_], ModuleL),
    module_links(Module1, Module2, Module3, [], PIL21, PIL23).
