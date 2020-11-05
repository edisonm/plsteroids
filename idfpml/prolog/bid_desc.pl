/*  CLP over binary integer decimal numbers

    Author:        Edison Mera Menendez
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

:- module(bid_desc, [bid_desc/2]).

bid_desc(pl_, [acos/2,   acosh/2, asin/2,   asinh/2, atan/2,
               atanh/2,  cbrt/2,  cos/2,    cosh/2,  erf/2,
               erfc/2,   exp10/2,  exp2/2,  exp/2,   expm1/2,
               lgamma/2, log10/2,  log1p/2, log2/2,  log/2,
               round_integral_exact/2,
               round_integral_nearest_away/2,
               round_integral_nearest_even/2,
               round_integral_negative/2,
               round_integral_positive/2,
               round_integral_zero/2,
               sin/2,    sinh/2,  sqrt/2,   tan/2,      tanh/2,
               tgamma/2, add/3,   atan2/3,  div/3,      fdim/3,
               hypot/3,  mul/3,   pow/3,    quantize/3, sub/3]).
bid_desc(pt_, [atom/2, string/2]).
bid_desc(pi_, [rnint/2,  xrnint/2, xrninta/2, int/2,  xint/2,
               floor/2,  xfloor/2, rninta/2,  ceil/2, xceil/2]).
bid_desc(is_, [quiet_equal/2,
               quiet_greater/2,
               quiet_greater_equal/2,
               quiet_greater_unordered/2,
               quiet_less/2,
               quiet_less_equal/2,
               quiet_less_unordered/2,
               quiet_not_equal/2,
               quiet_not_greater/2,
               quiet_not_less/2,
               quiet_ordered/2,
               quiet_unordered/2,
               signaling_greater/2,
               signaling_greater_equal/2,
               signaling_greater_unordered/2,
               signaling_less/2,
               signaling_less_equal/2,
               signaling_less_unordered/2,
               signaling_not_greater/2,
               signaling_not_less/2]).
bid_desc(pn_, [rem/3,
               fmod/3,
               minnum/3,
               maxnum/3,
               minnum_mag/3,
               maxnum_mag/3]).
