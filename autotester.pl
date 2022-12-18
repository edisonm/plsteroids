:- module(autotester, []).

:- user:[plsteroids].
:- use_module(library(plunit)).
:- [assertions/tests/'assertions.plt'].
:- [assertions/tests/foreign/'foreign.plt'].
:- [refactor/tests/'gcb.plt'].
:- [rtchecks/tests/'rtchecks.plt'].
:- [xlibrary/tests/i18n/'i18n_2.plt'].
:- [xlibrary/tests/i18n/'i18n.plt'].
:- [xtools/tests/'assrt_meta.plt'].
:- [stchecks/tests/'stchecks.plt'].
:- [library(gcover)].
:- [library(ws_cover)].
