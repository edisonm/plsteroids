:- begin_tests(termhide).

:- use_module(library(termhide)).
:- use_module(library(record_locations)).
:- use_module(library(comment_data)).
:- use_module(library(clambda)).
:- init_expansors.

:- set_setting(listing:tab_distance, 0). % Use only spaces, no tabs

:- comment_data:enable.

replace_noisy_strings(SD) -->
    replace_substrings(SD, ""),
    replace_substrings("ERROR: ", ""),
    {findall(L, between(2, 3, L), Ls)},
    {findall(C, between(0, 30, C), Cs)},
    foldl([Cs] +\ L^foldl(replace_substrings_lc(L), Cs), Ls).

replace_substrings_lc(L, C) -->
    { atomics_to_string(["termhide_example_", L, ".pl:", C, ":"], SubS),
      atomics_to_string(["termhide_example_", L, ".pl:xx:"], Repl)
    },
    replace_substrings(SubS, Repl).

replace_substrings(SubS, Repl, String, Result) :-
    ( sub_string(String, Before, _, After, SubS)
    ->sub_string(String, 0, Before, _, BeforeS),
      sub_string(String, _, After, 0, AfterS),
      replace_substrings(SubS, Repl, AfterS, Tail),
      string_concat(BeforeS, Repl, ResultHead),
      string_concat(ResultHead, Tail, Result)
    ; Result = String
    ).

/* $termhide$
termhide_example_2.pl:xx:
   Explicit usage of hidden term termhide_example:mykey(1) not allowed
termhide_example_2.pl:xx:
   Explicit usage of hidden term termhide_example:mykey(2) not allowed
termhide_example_3.pl:xx:
   Explicit usage of hidden term termhide_example:mykey(2) not allowed
*/

test(termhide) :-
    with_output_to(string(Result),
                   use_module(xlibrary/tests/termhide_example_3),
                   [capture([user_output, user_error])]),
    module_property(plunit_termhide, file(File)),
    directory_file_path(Dir, _, File),
    directory_file_path(Dir, '', AD),
    atom_string(AD, SD),
    comment_data(termhide, Pattern),
    replace_noisy_strings(SD, Result, AResult),
    assertion(Pattern == AResult).

:- end_tests(termhide).
