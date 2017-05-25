:- module(autotester, [cover_tests/0]).

:- user:[plsteroids].
:- [assertions/tests/'assertions.plt'].
:- [assertions/tests/foreign/'foreign.plt'].
:- [refactor/tests/'gcb.plt'].
:- [refactor/tests/'refactor.plt'].
:- [rtchecks/tests/'rtchecks.plt'].
:- [xlibrary/tests/i18n/'i18n_2.plt'].
:- [xlibrary/tests/i18n/'i18n.plt'].
:- [xtools/tests/'assrt_meta.plt'].
:- [xtools/tests/'ctchecks.plt'].
:- [library(gcover)].
:- [library(ws_cover)].

cover_tests :-
    setup_call_cleanup(
        plunit:setup_trap_assertions(Ref),
        cover_current_units,
        plunit:report_and_cleanup(Ref)).

cover_current_units :-
    working_directory(W,W),
    forall(plunit:current_test_set(Set),
           gcover(plunit:run_unit(Set), [tag(Set),
                                         file(directory_file_path(W,_))])),
    plunit:check_for_test_errors.
