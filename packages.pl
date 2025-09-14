:- dynamic
        package/1.

:- use_module(library(listing)).
:- use_module(library(settings)).
:- use_module(library(predicate_options)).

:- set_setting(listing:tab_distance, 0).

package(xlibrary).
package(assertions).
package(xtools).
package(rtchecks).
package(refactor).
% package(playground).
package(smtp).
package(clpcd).
package(lambda).
package(idfpml) :- \+ current_prolog_flag(processor, 'aarch64-linux').
package(decml).
package(mpfr).
package(mpc).
package(andorra).
package(stchecks).

packages(Packages) :- findall(Package, package(Package), Packages).

:- assertz(ref_msgtype:rstats_db).
