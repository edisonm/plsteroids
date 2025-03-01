clpcd
=====

Constraint logic programming over continuous domains

Based on the CLP(Q,R) package by Leslie De Koninck, K.U. Leuven, available in
SWI-Prolog.

Installation
------------

To install this package, just follow the next sequence of commands in your
SWI-Prolog shell:

```bash
  $ swipl
  
  ?- pack_install('https://github.com/edisonm/clpcd.git').
  true.
```

Overview
--------

The goal of this library is to make a CLP library for continuous domains like
reals or rationals, that is more appropriated for calculations found in
engineering applications.

Current CLP(QR) implementation works only when the system of equations are
linear, although in some cases, the equations can be modified so that they
become linear and make the system solvable. But also, it is not being maintained
and contains several unfortunate bugs, failing even for very small examples. It
also presents implementation limitations (like the linearity); probably because
the prototype was designed some decades ago, when computational power was
limited.

This library is focused to remove those limitations, and to incorporate
solvers for well-known non-linear equations, when such systems are identified.
The numeric resolution of such equations will be inspired or taken form existing
frameworks like the GNU Scientific Library (GSL) or GNU Octave.
