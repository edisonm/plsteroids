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
          [ depends_of/4,
            update_depends_of/2,
            module_links/6,
            unlink_loop/4
          ]).

:- use_module(library(calls_to)).
:- use_module(library(module_uses)).

ref_head('<assertion>'(M:H), M, H).
ref_head(M:H, M, H).
ref_head(clause(Ref), M, H) :-
    freeze(Ref,
           ( clause(M:P, _, Ref),
             ( P = dialog(H)
             ->true
             ; P = H
             )
           )).

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

depends_of(CH, CM, TH, TM) :-
    depends_of_db(CH, CM, IH, IM),
    ( TM:TH = IM:IH
    ; depends_of_rec(IH, IM, TH, TM)
    ).

depends_of_rec(CH, CM, TH, TM) :-
    depends_of(CH, CM, TH, TM),
    TM:TH \= CM:CH.

:- meta_predicate depends_of_cm(1,+,+,?,?).

depends_of_cm(Constraint, CH, CM, TH, TM) :-
    depends_of_db(CH, CM, IH, IM),
    call(Constraint, IM),
    ( TM:TH = IM:IH
    ; IM:IH \= CM:CH,
      depends_of_cm_rec(Constraint, IH, IM, TH, TM)
    ).

depends_of_cm_rec(Constraint, CH, CM, TH, TM) :-
    depends_of_cm(Constraint, CH, CM, TH, TM),
    TM:TH \= CM:CH.

member_of(List, Elem) :- memberchk(Elem, List).

:- meta_predicate dependent_cm(1,+,+,?,?).

dependent_cm(Constraint, TH, TM, CH, CM) :-
    depends_of_db(IH, IM, TH, TM),
    call(Constraint, IM),
    ( CM:CH = IM:IH
    ; IM:IH \= TM:TH,
      dependent_cm_rec(Constraint, IH, IM, CH, CM)
    ).

dependent_cm_rec(Constraint, TH, TM, CH, CM) :-
    dependent_cm(Constraint, TH, TM, CH, CM),
    TM:TH \= CM:CH.

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
    module_uses(Module1, Module2, PIL2),
    module_uses(Module2, Module3, PIL3),
    findall(F2/A2,
            ( member(F2/A2, PIL2),
              once(( member(F3/A3, PIL3),
                     functor(H2, F2, A2),
                     depends_of_cm(member_of([Module2, Module3]), H2, Module2, H3, Module3)
                   ))
            ), UPIL),
    findall(F2/A2,
            ( member(F3/A3, PIL3),
              functor(H3, F3, A3),
              dependent_cm(=(Module2), H3, Module3, H2, Module2),
              functor(H2, F2, A2)
            ), PIU23),
    sort(PIU23, PIL23),
    findall(F22/A22,
            ( member(F2/A2, PIL2),
              functor(H2, F2, A2),
              depends_of_cm(=(Module2), H2, Module2, H22, Module2),
              functor(H22, F22, A22)
            ), PIU21, PIL2),
    sort(PIU21, PIL21).

unlink_loop(ModuleL1, Module2, PIL21->Module1, PIL23->Module3) :-
    last(ModuleL1, Last),
    ModuleL1 = [First|_],
    append([Last|ModuleL1], [First], ModuleL),
    append(_, [Module1, Module2, Module3|_], ModuleL),
    module_links(Module1, Module2, Module3, [], PIL21, PIL23).
