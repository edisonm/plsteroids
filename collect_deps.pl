:- module(collect_deps, [collect_deps/1, reduce_tree/2]).

:- use_module(plsconfig).
:- [plsloader].

:- use_module(library(apply)).

collect_deps(Loaded, Pack, Pack) :-
    memberchk(Pack, Loaded),
    !,
    print_message(error, format("Circular dependency not allowed ~w", [[Pack|Loaded]])).
collect_deps(Loaded, Pack, Pack-TreeL) :-
    absolute_file_name(Pack/pack, F, [file_type(prolog)]),
    read_file(F, PackOptions),
    findall(ReqPack, member(requires(ReqPack), PackOptions), PackL),
    maplist(collect_deps([Pack|Loaded]), PackL, TreeL).

collect_deps(TreeL) :-
    packages(Packages),
    maplist(collect_deps([]), Packages, FullTreeL),
    reduce_tree(FullTreeL, TreeL).

reduce_tree(FullTreeL1, TreeL) :-
    ( select(Tree, FullTreeL1, FullTreeL),
      sub_term(Tree, FullTreeL)
    ->reduce_tree(FullTreeL, TreeL1)
    ; TreeL1 = FullTreeL1
    ),
    maplist(reduce_tree_, TreeL1, TreeL).

reduce_tree_(Node-FullTreeL, Node-TreeL) :- reduce_tree(FullTreeL, TreeL).
