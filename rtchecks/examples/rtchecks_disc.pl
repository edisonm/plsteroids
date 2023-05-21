:- module(rtchecks_disc, [disc1/1]).

:- use_module(library(assertions)).
:- use_module(library(plprops)).
:- init_expansors.

:- discontiguous disc1/1.

:- pred disc1(A) : (A=a).
disc1(a).
:- pred disc1(A) : (A=b).
foo.
disc1(b).
:- pred disc1(A) : (A=c).
disc1(c) :- foo.
