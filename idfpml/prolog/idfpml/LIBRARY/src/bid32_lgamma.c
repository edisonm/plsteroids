/******************************************************************************
  Copyright (c) 2007-2024, Intel Corp.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright notice,
      this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of Intel Corporation nor the names of its contributors
      may be used to endorse or promote products derived from this software
      without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
  CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
  SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
  INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
  CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
  ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF
  THE POSSIBILITY OF SUCH DAMAGE.
******************************************************************************/

#include "bid_internal.h"
double lgamma(double);
double fabs(double);
double log(double);
double sin(double);

#define BID32_INF 0x78000000ul 

BID_TYPE0_FUNCTION_ARGTYPE1(BID_UINT32, bid32_lgamma, BID_UINT32, x)

// Declare local variables

  BID_UINT32 res, x_int, x_frac;
  double xd, yd, fd;
  int cmp_res;

// Check for NaN and just return the same NaN, quieted and canonized

  if ((x & NAN_MASK32) == NAN_MASK32)
   {
     #ifdef BID_SET_STATUS_FLAGS
     if ((x & SNAN_MASK32) == SNAN_MASK32)
        __set_status_flags (pfpsf, BID_INVALID_EXCEPTION);
     #endif
     res = x & 0xfc0ffffful;
     if ((res & 0x000ffffful) > 999999ul) res &= ~0x000ffffful;
     BID_RETURN(res);
   }

// Convert to binary

  BIDECIMAL_CALL1(bid32_to_binary64,xd,x);

// If x >= 1/2 then we're very safe doing the operation naively.
// This applies even to the case x = +inf where lgamma(x) = +inf

  if (xd >= 0.5)
   { yd = lgamma(xd);
     BIDECIMAL_CALL1(binary64_to_bid32,res,yd);
     BID_RETURN (res);
   }

// Filter out the case of negative infinity, where we return +inf

  BIDECIMAL_CALL1_NORND_NOSTAT(bid32_isInf,cmp_res,x);
  if (cmp_res)
   { res = BID32_INF;
     BID_RETURN (res);
   }

// Otherwise, even with the huge extra precision, we may need to worry
// about the singularities at nonnegative integers. So we use the reflection
// formula
//
// Gamma(x) = pi / (sin (pi * x) * Gamma(1 - x))
// log|Gamma(x)| = log pi - lgamma(1 - x) - log|sin(pi * x)|
// Form the integer and fractional parts of x, and convert fractional
// part to double.

  BIDECIMAL_CALL1_NORND(bid32_round_integral_nearest_even, x_int, x);
  BIDECIMAL_CALL2(bid32_sub,x_frac,x,x_int);

// If the fractional part is 0, return +inf

  BIDECIMAL_CALL1_NORND_NOSTAT(bid32_isZero,cmp_res,x_frac);
  if (cmp_res)
   { res = BID32_INF;
      #ifdef BID_SET_STATUS_FLAGS
        __set_status_flags (pfpsf, BID_ZERO_DIVIDE_EXCEPTION);
     #endif
     BID_RETURN (res);
   }

// Otherwise do the main computation in double.

  BIDECIMAL_CALL1(bid32_to_binary64,fd,x_frac);
  yd = 1.144729885849400174143 -
       log(fabs(sin(3.14159265358979323846 * fd))) -
       lgamma(1.0 - xd);
  BIDECIMAL_CALL1(binary64_to_bid32,res,yd);
  BID_RETURN (res);
}
