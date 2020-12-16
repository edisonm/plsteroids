#ifndef __pl_bid_H
#define __pl_bid_H

#include <bid_internal.h>
#include <bid_functions.h>
#include <SWI-Stream.h>
#include <SWI-Prolog.h>

typedef BID_UINT64  bid64_t;
typedef BID_UINT128 bid128_t;

#define FI_get_bid64_t(_,  t, v) PL_get_bid64_t( (t), (v))
#define FI_get_bid128_t(_, t, v) PL_get_bid128_t((t), (v))

int FI_unify_bid64_t(term_t t, bid64_t * const b);
int FI_unify_bid128_t(term_t t, bid128_t * const b);
int PL_get_bid64_t( term_t t, bid64_t *b);
int PL_get_bid128_t(term_t t, bid128_t *b);

#endif
