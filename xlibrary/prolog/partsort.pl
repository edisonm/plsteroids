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

:- module(partsort,
          [ partsort/3
          ]).

:- use_module(library(lists)).

:- meta_predicate partsort(3,+,-).

%!  partsort(:Pred, +List, -Sorted) is det.
%
%   Partial sort.  Used to sort a list when the order can be determined only for
%   > and  =, and  not for all  the elements.  For  instance, when  ordering the
%   branches of a tree in such a way that the parents appear first, two nodes of
%   different  branches  are  unrelated,  but children  are  higher  than  their
%   parents. Note that if used with the compare/3 predicate, it is equivalent to
%   predsort/3 implemented with the selection sort algorithm.

partsort(Pred, List1, Sorted1) :-
    select(E1, List1, List),
    ( member(E2, List),
      call(Pred, =, E1, E2)
    ->Sorted = Sorted1
    ; member(E2, List),
      call(Pred, >, E1, E2)
    ->fail
    ; Sorted1 = [E1|Sorted]
    ),
    !,
    partsort(Pred, List, Sorted).
partsort(_, List, List).
