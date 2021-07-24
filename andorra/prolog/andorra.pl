/*  Andorra execution

    Author:        Claudio Vaucheret, Francisco Bueno.  Ported by Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           http://www.edisonm.com
    Copyright (C): 2019, Process Design Center, Breda, The Netherlands.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(andorra, []).

:- reexport(library(compound_expand)).
:- reexport(library(andorra_op)).
:- reexport(library(andorra_rt)).
:- reexport(library(andorra_builtins)).
:- use_module(library(andorra_tr)).

term_expansion(Term1, Term) :-
    andorra_term_expansion(Term1, Term).

/** <module> Andorra execution

This package allows the execution under the Basic Andorra Model
@cite{andorra-principle}. The model classifies goals as a
@index{determinate goal}, if at most one clause matches the goal, or
nondeterminate goal, otherwise. In this model a goal is delayed until
either it becomes determinate or it becomes the leftmost goal and no
determinate goal is available. The implementation of this selection
rule is based on the use of attributed variables
@cite{holzbaur-plilp92,holzbaur-phd}.

In order to test determinacy we verify only the heads of clauses and
builtins in the bodies of clauses before the first cut, if any. 
By default, determinacy of a goal is detected dynamically: when called,
if at most one clause matches, it is executed; otherwise, it is 
delayed. For goals delayed the test is repeated each time a variable 
appearing in the goal is instantiated.
In addition, efficiency can be improved by using declarations
that specify the determinacy conditions. These will be considered
for testing instead of the generic test on all clauses that can match.

As with any other Ciao package, the andorra computation rule affects
only the module that uses the package. If execution passes across
two modules that use the computation rule, determinate goals are run
in advance @em{within} one module and also within the other module.
But determinate goals of one module do not run ahead of goals of the
other module.

It is however possible to preserve the computation rule for calls to
predicates defined in other modules. These modules should obviously also
use this package. In addition @em{all} predicates from such modules should
imported, i.e., the directive @tt{:- use_module(module)}, should be used in
this case instead of @tt{:- use_module(module,[...])}.  Otherwise calls to
predicates outside the module will only be called  when they became the leftmost goal.

The andorra transformation will include the following predicates into the code
of the module that uses the package. Be careful not to define predicates by
these names:

- **detcond_andorra/4**
- **path_andorra/4**
- **detcond_susp/4**
- **path_susp/4**
- **list_andorra2/5**
- **test_andorra2/4**

*/
