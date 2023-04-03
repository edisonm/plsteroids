:- module(c4, [q/1]).

:- use_module(c2).
:- init_expansors.

q(X) :-
    p1(X).

p1(d).

% before static optimization:
% time(q(x)).
% % 95 inferences, 0.000 CPU in 0.000 seconds (94% CPU, 3400143 Lips)

% after static optimization:
% time(q(x)).
% % 16 inferences, 0.000 CPU in 0.000 seconds (94% CPU, 2160119 Lips)
