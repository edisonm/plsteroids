:- module(gen_psg,
          [ gen_psg/0
          ]).

:- use_module(library(solution_sequences)).

gen_psg :-
    writeln("(defconst prolog-smie-grammar"),
    writeln("  '((\".\" -999 -999)"),
    forall(
        order_by([desc(A)],
                 ( current_op(A, B, C),
                   \+ member(C, ['.', '$', '\\']),
                   atom_string(C, S),
                   ( member(B,[yfx,xfy,xfx]),
                     format(atom(Txt), '    (~q -~w -~w)~n', [S, A, A])
                   ; member(B,[fx,fy]),
                     \+ member(C, [+, -]),
                     format(atom(Txt), '    (~q nil -~w)~n', [S, A])
                   )
                 )),
        write(Txt)),
    writeln("    (:smie-closer-alist (t . \".\"))"),
    writeln("    )"),
    writeln("  \"Precedence levels of infix operators.\")").
