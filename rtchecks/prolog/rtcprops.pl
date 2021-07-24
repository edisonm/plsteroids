/*  Part of Run-Time Checker for Assertions

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/refactor
    Copyright (C): 2017, Process Design Center, Breda, The Netherlands.
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

:- module(rtcprops, [acheck/1, acheck/2, acheck/3, no_acheck/1, no_acheck/2]).

:- use_module(library(assertions)).
:- use_module(library(metaprops)).
:- use_module(library(globprops)).

:- true comp [functor/3,
              predicate_property/2,
              current_predicate/2,
              (>)/2,
              (<)/2,
              (>=)/2,
              (=<)/2,
              (=\=)/2,
              (=:=)/2,
              (is)/2,
              atomic_list_concat/2,
              atomic_list_concat/3,
              atom_number/2,
              atom_codes/2,
              sub_atom/5,
              maplist/2,
              maplist/3,
              maplist/4,
              maplist/5,
              memberchk/2] + no_acheck(rt).

:- type acstatus/1.

%!  acstatus(Status)
%
%   Status of the assertion checker for a given property. Valid values are:
%
%   - unimplemented: No run-time checker has been implemented for the property.
%                    Althought it can be implemented further.
%
%   - incomplete: The current run-time checker is incomplete, which means, under
%                 certain circunstances, no error is reported if the property is
%                 violated.
%
%   - unknown: We do not know if current implementation of run-time checker is
%              complete or not.
%
%   - complete: The opposite of incomplete, if the property is violated, error
%               is always reported. Default.
%
%   - impossible: The property must not be run-time checked (for theoretical or
%                 practical reasons).
%
%

acstatus(unimplemented).
acstatus(incomplete).
acstatus(complete).
acstatus(unknown).
acstatus(exhaustive).
acstatus(impossible).

:- type ctrt/1.

ctrt(ct).
ctrt(rt).

:- global acheck(T, Status, G) : ctrt * acstatus * callable
    # "The ~w assertion check of ~w has the status ~w."-[T, G, Status].

acheck(_, _, Goal) :- call(Goal).

:- global acheck(T, G) + equiv(acheck(T, complete, G))
   # "Equivalent to acheck(~w, complete, ~w)."-[T, G].

acheck(_, Goal) :- call(Goal).

:- global acheck(G) + equiv(acheck(ct, acheck(rt, G))).

acheck(Goal) :- call(Goal).

:- global no_acheck(T, G) + equiv(acheck(T, impossible, G))
    # "Declares that the assertion in which this comp property appears must not
    be checked at run-time.".

no_acheck(_, Goal) :- call(Goal).

:- global no_acheck(G) + equiv(noacheck(ct, noacheck(rt, G))).

no_acheck(Goal) :- call(Goal).
