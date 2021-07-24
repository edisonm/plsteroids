/*  CLP over binary integer decimal numbers

    Author:        Edison Mera
    E-mail:        efmera@gmail.com
    WWW:           https://github.com/edisonm/assertions
    Copyright (C): 2020, Process Design Center, Breda, The Netherlands.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/

:- module(bid_desc, [bid_desc/3]).

bid_desc(pl_, [acos,   acosh, asin,   asinh, atan,
               atanh,  cbrt,  cos,    cosh,  erf,
               erfc,   exp10,  exp2,  exp,   expm1,
               lgamma, log10,  log1p, log2,  log,
               round_integral_exact,
               round_integral_nearest_away,
               round_integral_nearest_even,
               round_integral_negative,
               round_integral_positive,
               round_integral_zero,
               sin,    sinh,  sqrt,   tan,      tanh,
               tgamma], 2).
bid_desc(pl_, [add,   atan2,  div,      fdim,
               hypot,  mul,   pow,    quantize, sub], 3).
bid_desc(pn_, [rem,
               fmod,
               minnum,
               maxnum,
               minnum_mag,
               maxnum_mag], 3).
bid_desc(pt_, [atom, string], 2).
bid_desc(pi_, [rnint,  xrnint, xrninta, int,  xint,
               floor,  xfloor, rninta,  ceil, xceil], 2).
bid_desc(is_, [quiet_equal,
               quiet_greater,
               quiet_greater_equal,
               quiet_greater_unordered,
               quiet_less,
               quiet_less_equal,
               quiet_less_unordered,
               quiet_not_equal,
               quiet_not_greater,
               quiet_not_less,
               quiet_ordered,
               quiet_unordered,
               signaling_greater,
               signaling_greater_equal,
               signaling_greater_unordered,
               signaling_less,
               signaling_less_equal,
               signaling_less_unordered,
               signaling_not_greater,
               signaling_not_less], 2).
