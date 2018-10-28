/*  $Id$

    Part of CLP(Q) (Constraint Logic Programming over Rationals)

    Author:        Leslie De Koninck
    E-mail:        Leslie.DeKoninck@cs.kuleuven.be
    WWW:           http://www.swi-prolog.org
		   http://www.ai.univie.ac.at/cgi-bin/tr-online?number+95-09
    Copyright (C): 2006, K.U. Leuven and
		   1992-1995, Austrian Research Institute for
		              Artificial Intelligence (OFAI),
			      Vienna, Austria

    This software is based on CLP(Q,R) by Christian Holzbaur for SICStus
    Prolog and distributed under the license details below with permission from
    all mentioned authors.

    This program is free software; you can redistribute it and/or
    modify it under the terms of the GNU General Public License
    as published by the Free Software Foundation; either version 2
    of the License, or (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301  USA

    As a special exception, if you link this library with other files,
    compiled with a Free Software compiler, to produce an executable, this
    library does not by itself cause the resulting executable to be covered
    by the GNU General Public License. This exception does not however
    invalidate any other reasons why the executable file might be covered by
    the GNU General Public License.
*/

:- module(bb_q,
	[
	]).

:- use_module(library(clpcd/nf)).

% bb_narrow_lower(X)
%
% Narrows the lower bound so that it is an integer bound.
% We do this by finding the infimum of X and asserting that X
% is larger than the first integer larger or equal to the infimum
% (second integer if X is to be strict larger than the first integer).

bb_narrow_lower(X) :-
	(   inf(X,Inf)
	->  Bound is ceiling(Inf),
	    (   entailed(CLP, X > Bound)
	    ->  {X >= Bound+1}
	    ;   {X >= Bound}
	    )
	;   true
	).

% bb_narrow_upper(X)
%
% See bb_narrow_lower/1. This predicate handles the upper bound.

bb_narrow_upper(X) :-
	(   sup(X,Sup)
	->  Bound is floor(Sup),
	    (   entailed(X < Bound)
	    ->  {X =< Bound-1}
	    ;   {X =< Bound}
	    )
	;   true
	).

		 /*******************************
		 *	       SANDBOX		*
		 *******************************/
:- multifile
	sandbox:safe_primitive/1.

sandbox:safe_primitive(bb_q:bb_inf(_,_,_)).
sandbox:safe_primitive(bb_q:bb_inf(_,_,_,_)).
