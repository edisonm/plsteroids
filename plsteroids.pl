:- dynamic
        package/1.

:- use_module(library(listing)).
:- set_setting(listing:tab_distance, 0).

package(xlibrary).
package(assertions).
package(xtools).
package(rtchecks).
package(refactor).
% Next package should not be installed, since it contains experimental code:
package(playground).

:- [plsdirs].
:- [pltools].

:- assertz(ref_msgtype:rstats_db).
