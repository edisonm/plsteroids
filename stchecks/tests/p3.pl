:- module(p3, []).

:- use_module(library(assertions)).
:- use_module(library(plprops)).
:- use_module(library(rtchecks)).
:- init_expansors.

:- rtcheck p/1.

:- pred p(atm).

p(a).
p(b).
p(c).
