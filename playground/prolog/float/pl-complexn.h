#ifndef __pl_complexn_H
#define __pl_complexn_H

#include <stdio.h>
#include <mpc.h>
#include <SWI-Stream.h>
#include <SWI-Prolog.h>

typedef mpc_t complexn;

#define FI_unify_complexn(t, v) PL_unify_complexn((t), (v))
#define FI_get_complexn(_, t, v) PL_get_complexn((t), (v))

int PL_unify_complexn(term_t t, complexn *fr);
int PL_get_complexn(term_t t, complexn **fr);

#endif
