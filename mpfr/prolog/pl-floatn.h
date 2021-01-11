#ifndef __pl_floatn_H
#define __pl_floatn_H

#include <stdio.h>
#include <mpfr.h>
#include <SWI-Stream.h>
#include <SWI-Prolog.h>

typedef mpfr_t floatn_t;

PL_blob_t *record_mpfr;

#define FI_unify_floatn_t(t, v) PL_unify_floatn_t((t), (v))
#define FI_get_floatn_t(_, t, v) PL_get_floatn_t((t), (v))
#define FI_unify_mpfr_prec_t(t, v) FI_unify_integer((t), (v))
#define FI_get_mpfr_prec_t(r, t, v) FI_get_long((r), (t), (v))

int PL_unify_floatn_t(term_t t, floatn_t *fr);
int PL_get_floatn_t(term_t t, floatn_t **fr);
int floatn_out_str(IOSTREAM *stream, floatn_t *ref);
void floatn_init();

#define MAX(a, b) ((a) > (b) ? (a) : (b))

#endif
