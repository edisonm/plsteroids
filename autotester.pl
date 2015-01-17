:- [xlibrary/tests/i18n/'i18n.plt'].
:- [assertions/tests/'assertions.plt'].
:- [assertions/tests/foreign/'foreign.plt'].
:- [refactor/tests/'gcb.plt'].
:- [refactor/tests/'refactor.plt'].
:- [rtchecks/tests/'ctchecks.plt'].
:- [library(gcover)].

cover_tests :-
    working_directory(W,W),
    gcover(run_tests, [file(directory_file_path(W,_))]).
