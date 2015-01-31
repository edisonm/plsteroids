:- module(autotester, [cover_tests/0]).

:- [xlibrary/tests/i18n/'i18n.plt'].
:- [assertions/tests/'assertions.plt'].
:- [assertions/tests/foreign/'foreign.plt'].
:- [refactor/tests/'gcb.plt'].
:- [rtchecks/tests/'ctchecks.plt'].
:- [rtchecks/tests/'rtchecks.plt'].
:- [refactor/tests/'refactor.plt'].
:- [library(gcover)].
:- [library(ws_cover)].

cover_tests :-
    working_directory(W,W),
    gcover(run_tests, [file(directory_file_path(W,_))]).
