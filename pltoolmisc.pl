:- module(pltoolmisc, [checkhelp/0,
                       updatedoc/1]).

:- use_module(library(unix)).
:- use_module(library(atomics_string)).

user:message_property(_, stream(current_output)).

updatedoc(File) :-
    read_file_to_codes(File, Codes, []),
    string_codes(String, Codes),
    expand_string(String, ExpandL, []),
    tell(File),
    forall(member(E, ExpandL), format("~s", [E])),
    told.

checkhelp :-
    with_output_to(string(S),
                   checkhelp_1),
    atomic_list_concat(L, "\n% ", S),
    forall(member(E, L), format("~s~n", [E])).

checkhelp_1 :-
    forall(available_checker(C),
           ( format("~w:~n", [C]),
             print_message(information, acheck(C))
           )).

expand_string(String) -->
    ( {atomics_string([Pre, "[//]: # (prolog_ini ", Command, ")\n",
                      _,   "[//]: # (prolog_end)\n", Post], String)}
    ->[Pre, "[//]: # (prolog_ini ", Command, ")\n"],
      { term_string(Term, Command),
        with_output_to(string(Repl), Term)
      },
      ["[//]: # (Content generated automatically by pltoolmisc.pl)\n",
       Repl, "[//]: # (prolog_end)\n"],
      expand_string(Post)
    ; [String]
    ).
