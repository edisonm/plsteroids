stchecks
========

Static Checks for Prolog programs and Assertions

Static Analysis
===============

A static analysis tool mostly based on looking the abstract syntax graph for
inconsistencies, for example:


```
 ?- [library(checkers)].
true.
 ?- checkall([dir('.')]).
```

will execute all the checkers to analyze the modules in the directory
'.'. Alternatively you can use checkallc instead, which is the concurrent
version of the above.

If you want to use a specific analysis, use the predicate showcheck(Analysis,
Options), where Analsyis can be:


[//]: # (prolog_ini checkhelp)
[//]: # (Content generated automatically by pltoolmisc.pl)
useless_cuts:
Check useless cuts
------------------
The predicates contain cuts that are actually not needed.
That could happen when the analysis determined that all
the calls before the cut and the clause itself where
deterministic or they have one of the next properties:
det, semidet, is_det or fails.  Note that in recursive
predicates the cut would look as needed, but it is not
since it can be removed or avoided via refactoring.

assertions:
Check asssertions
-----------------
The predicates contain assertions that are inconsistent
with the implementation.

deprecated:
Deprecated Predicates
---------------------
The predicates below are marked as deprecated, so you have to
avoid its usage in new code, and to refactorize old code.

dupcode:
Duplicated Code
---------------
The elements below would has been implemented in different modules,
but are duplicates.  Would be a symptom of duplicated functionality.
In the case of predicate names, at least one has been exported,
making difficult to import it in other modules without clash risk.
This can be fixed by merging the duplicated code, or by refactoring
one of the duplicated to avoid this warning. Note that predicates
declared as public are ignored by this analysis.

imports:
Unused Imports
--------------
The predicates or modules below has been imported, however they
are never used in the importing module, or they do not implement
new clauses for multifile predicates.  Note that modules that
export operators, or that do not export any predicate are not
reported.
You can silent the warnings by declaring use_module/2 with an
empty import list. If they have desirable side effects and still
needs to be imported, you can refactorize your program so that
such side effects are not required anymore.

meta_decls:
Missing Meta Predicate Declarations
-----------------------------------
The predicates below require a missing meta_predicate declaration.
They have been automatically inferred. Although is not required, it
is recommended to add them by hand or to fix the predicate in order
to facilitate static analysis and refactoring.

non_loaded:
Non Loaded
----------
The following files are not being loaded, which
means you are not analyzing them statically

non_mutually_exclusive:
Non Mutually Exclusive Predicates
---------------------------------
The predicates below are marked as mutually_exclusive, but they have
non mutually exclusive clauses. You can resolve the ambiguity unifying
the non mutual exclusive clauses or changing the specification of such
predicates.

trivial_fails:
Trivial Fails
-------------
The literals below always fails, due to there are no
matching clauses for such calls, which is reported as
a trivial fail, or because all paths leads to dead
points, in such case the warning reports also the
biggest failure chain found

undefined:
Undefined Predicates
--------------------
The predicates below are not defined. If these are defined
at runtime using assert/1, use :- dynamic Name/Arity.

unused:
Unused Predicates
-----------------
The predicates has been implemented, however they are
never referenced in the code nor exported.  Probably are
dead-code, part of an incomplete implementation, or called
indirectly by some meta predicate without or with incorrect
meta_predicate declaration.  In any case this represents a
bad design and must be fixed, either completing the program
or removing the unreferenced predicates.

wrong_dynamic:
Wrong Dynamic Declarations
--------------------------
The predicates present inconsistencies between its
usage and the dynamic declarations. Could be that they are
being used as dynamic without a proper declaration, being
declared as dynamic but never asserted, retracted, or using
a variable argument in a database predicate, making it
difficult to analyze.

module_loops:
Module loops
------------
Module loops could potentially lead to Demeter's law violations


[//]: # (prolog_end)

