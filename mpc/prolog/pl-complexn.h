#ifndef __pl_complexn_H
#define __pl_complexn_H

#include <stdio.h>
#include <mpc.h>
#include <SWI-Stream.h>
#include <SWI-Prolog.h>

typedef mpc_t complexn_t;

#define FI_unify_complexn_t(t, v) PL_unify_complexn_t((t), (v))
#define FI_get_complexn_t(_, t, v) PL_get_complexn_t((t), (v))

int PL_unify_complexn_t(term_t t, complexn_t *fr);
int PL_get_complexn_t(term_t t, complexn_t **fr);

#endif
