#include "pl-floatn.h"
#include "pl-complexn.h"
#include <string.h>
#include <dlfcn.h>
#include <foreign_interface.h>

void complexn_to_str(complexn *ref, char **c)
{
    size_t size;
    FILE *stream = open_memstream(c, &size);
    mpc_out_str(stream, 10, 0, *ref, MPC_RNDNN);
    fclose(stream);
}

static int write_complexn_ref(IOSTREAM *s, atom_t aref, int flags)
{
    complexn *ref = PL_blob_data(aref, NULL, NULL);
    (void) flags;
    char *c;
    complexn_to_str(ref, &c);
    Sfprintf(s, "%s", c);
    free(c);

    return TRUE;
}

static int release_complexn(atom_t aref)
{
    complexn *ref = PL_blob_data(aref, NULL, NULL);
    char *c;
    complexn_to_str(ref, &c);
    mpc_clear(*ref);
    free(ref);
    free(c);

    return TRUE;
}

static void aquire_complexn(atom_t aref)
{
    mpc_t *ref = PL_blob_data(aref, NULL, NULL);
    (void) ref;
}

static PL_blob_t record_mpc =
{
    PL_BLOB_MAGIC,
    //PL_BLOB_UNIQUE|
    PL_BLOB_NOCOPY,
    "complexn",
    release_complexn,
    NULL,
    write_complexn_ref,
    aquire_complexn
};

foreign_t is_complexn(term_t v) {
    void *src;
    PL_blob_t *type;
    return PL_get_blob(v, (void *)&src, NULL, &type)
        && type == &record_mpc;
}

int is_mpc_prec_t(term_t v) {
    return PL_is_integer(v);
}

#define COMPLEXN_FUNCTION0(name)                                        \
    foreign_t complexn_##name(term_t p_r, term_t p_i, term_t r)         \
    {                                                                   \
        complexn *cx;                                                   \
        PL_blob_t *type;                                                \
        mpfr_prec_t prec_r, prec_i;                                     \
        cx = malloc(sizeof(complexn));                                  \
        if (!PL_get_long(p_r, &prec_r))                                 \
            prec_r = mpfr_get_default_prec();                           \
        if (!PL_get_long(p_i, &prec_i))                                 \
            prec_i = mpfr_get_default_prec();                           \
        mpc_init3(*cx, prec_r, prec_i);                                 \
        mpc_##name(*cx, MPC_RNDNN);                                     \
        return PL_unify_complexn(r, cx);                                \
    }

#define COMPLEXN_FUNCTION0n(name)                                       \
    foreign_t complexn_##name(term_t p_r, term_t p_i, term_t r)         \
    {                                                                   \
        complexn *cx;                                                   \
        PL_blob_t *type;                                                \
        mpfr_prec_t prec_r, prec_i;                                     \
        cx = malloc(sizeof(complexn));                                  \
        if (!PL_get_long(p_r, &prec_r))                                 \
            prec_r = mpfr_get_default_prec();                           \
        if (!PL_get_long(p_i, &prec_i))                                 \
            prec_i = mpfr_get_default_prec();                           \
        mpc_init3(*cx, prec_r, prec_i);                                 \
        mpc_##name(*cx);                                                \
        return PL_unify_complexn(r, cx);                                \
    }

#define COMPLEXN_FUNCTION1(name)                                        \
    foreign_t complexn_##name(term_t a, term_t p_r, term_t p_i, term_t r) \
    {                                                                   \
        complexn *cx;                                                   \
        PL_blob_t *type;                                                \
        mpfr_prec_t prec_ar, prec_r, prec_i, prec_ai;                   \
        complexn *ra;                                                   \
        __rtcheck(PL_get_complexn(a, &ra));                             \
        cx = malloc(sizeof(complexn));                                  \
        mpc_get_prec2(&prec_ar, &prec_ai, *ra);                         \
        if (!PL_get_long(p_r, &prec_r))                                 \
            prec_r = prec_ar;                                           \
        if (!PL_get_long(p_i, &prec_i))                                 \
            prec_i = prec_ai;                                           \
        mpc_init3(*cx, prec_r, prec_i);                                 \
        mpc_##name(*cx, *ra, MPC_RNDNN);                                \
        return PL_unify_complexn(r, cx);                                \
    }

#define FLOATN_FUNCTION1(name)                                          \
    foreign_t complexn_##name(term_t a, term_t p_r, term_t p_i, term_t r) \
    {                                                                   \
        floatn *fr;                                                     \
        PL_blob_t *type;                                                \
        mpfr_prec_t prec_r, prec_i, prec_ar, prec_ai;                   \
        complexn *ra;                                                   \
        __rtcheck(PL_get_complexn(a, &ra));                             \
        fr = malloc(sizeof(floatn));                                    \
        mpc_get_prec2(&prec_ar, &prec_ai, *ra);                         \
        if (!PL_get_long(p_r, &prec_r))                                 \
            prec_r = prec_ar;                                           \
        if (!PL_get_long(p_i, &prec_i))                                 \
            prec_i = prec_ai;                                           \
        mpfr_init2(*fr, MAX(prec_r, prec_i));                           \
        mpc_##name(*fr, *ra, MPFR_RNDN);                                \
        return PL_unify_floatn(r, fr);                                  \
    }

#define COMPLEXN_FUNCTION2(name)                                        \
    foreign_t complexn_##name(term_t a, term_t b, term_t p_r, term_t p_i, term_t r) \
    {                                                                   \
        complexn *cx;                                                   \
        PL_blob_t *type;                                                \
        mpfr_prec_t prec_r, prec_i, prec_ar, prec_ai, prec_br, prec_bi; \
        complexn *ra, *rb;                                              \
        __rtcheck(PL_get_complexn(a, &ra));                             \
        __rtcheck(PL_get_complexn(b, &rb));                             \
        cx = malloc(sizeof(complexn));                                  \
        mpc_get_prec2(&prec_ar, &prec_ai, *ra);                         \
        mpc_get_prec2(&prec_br, &prec_bi, *rb);                         \
        if (!PL_get_long(p_r, &prec_r))                                 \
            prec_r = MAX(prec_ar, prec_br);                             \
        if (!PL_get_long(p_i, &prec_i))                                 \
            prec_i = MAX(prec_ai, prec_bi);                             \
        mpc_init3(*cx, prec_r, prec_i);                                 \
        mpc_##name(*cx, *ra, *rb, MPC_RNDNN);                           \
        return PL_unify_complexn(r, cx);                                \
    }

#define COMPLEXN_FUNCTION2f(name)                                       \
    foreign_t complexn_##name(term_t a, term_t b, term_t p_r, term_t p_i, term_t r) \
    {                                                                   \
        complexn *cx;                                                   \
        PL_blob_t *type;                                                \
        mpfr_prec_t prec_r, prec_i;                                     \
        floatn *ra, *rb;                                                \
        __rtcheck(PL_get_floatn(a, &ra));                               \
        __rtcheck(PL_get_floatn(b, &rb));                               \
        cx = malloc(sizeof(complexn));                                  \
        if (!PL_get_long(p_r, &prec_r))                                 \
            prec_r = mpfr_get_prec(*ra);                                \
        if (!PL_get_long(p_i, &prec_i))                                 \
            prec_i = mpfr_get_prec(*rb);                                \
        mpc_init3(*cx, prec_r, prec_i);                                 \
        mpc_##name(*cx, *ra, *rb, MPC_RNDNN);                           \
        return PL_unify_complexn(r, cx);                                \
    }

/* not available, some to implement in prolog
COMPLEXN_FUNCTION1(cbrt)
COMPLEXN_FUNCTION1(log2)
COMPLEXN_FUNCTION1(log1p)
COMPLEXN_FUNCTION1(exp2)
COMPLEXN_FUNCTION1(expm1)
COMPLEXN_FUNCTION1(sec)
COMPLEXN_FUNCTION1(csc)
COMPLEXN_FUNCTION1(cot)
COMPLEXN_FUNCTION1(exp10)
COMPLEXN_FUNCTION1(sech)
COMPLEXN_FUNCTION1(csch)
COMPLEXN_FUNCTION1(coth)
COMPLEXN_FUNCTION1(eint)
COMPLEXN_FUNCTION1(li2)
COMPLEXN_FUNCTION1(gamma)
COMPLEXN_FUNCTION1(lngamma)
COMPLEXN_FUNCTION1(digamma)
COMPLEXN_FUNCTION1(zeta)
COMPLEXN_FUNCTION1(erf)
COMPLEXN_FUNCTION1(erfc)
COMPLEXN_FUNCTION1(j0)
COMPLEXN_FUNCTION1(j1)
COMPLEXN_FUNCTION1(y0)
COMPLEXN_FUNCTION1(y1)
COMPLEXN_FUNCTION1(ai)
*/
COMPLEXN_FUNCTION0n(set_nan)

COMPLEXN_FUNCTION1(sqrt)
COMPLEXN_FUNCTION1(neg)
COMPLEXN_FUNCTION1(log)
COMPLEXN_FUNCTION1(log10)
COMPLEXN_FUNCTION1(exp)
COMPLEXN_FUNCTION1(cos)
COMPLEXN_FUNCTION1(sin)
COMPLEXN_FUNCTION1(tan)
COMPLEXN_FUNCTION1(acos)
COMPLEXN_FUNCTION1(asin)
COMPLEXN_FUNCTION1(atan)
COMPLEXN_FUNCTION1(cosh)
COMPLEXN_FUNCTION1(sinh)
COMPLEXN_FUNCTION1(tanh)
COMPLEXN_FUNCTION1(acosh)
COMPLEXN_FUNCTION1(asinh)
COMPLEXN_FUNCTION1(atanh)
COMPLEXN_FUNCTION1(proj)

FLOATN_FUNCTION1(abs)
FLOATN_FUNCTION1(arg)
FLOATN_FUNCTION1(norm)
FLOATN_FUNCTION1(real)
FLOATN_FUNCTION1(imag)

COMPLEXN_FUNCTION2(add)
COMPLEXN_FUNCTION2(mul)
COMPLEXN_FUNCTION2(sub)
COMPLEXN_FUNCTION2(div)
COMPLEXN_FUNCTION2(pow)
COMPLEXN_FUNCTION2f(set_fr_fr)

/* COMPLEXN_FUNCTION2i2(rootn_ui) */
/* COMPLEXN_FUNCTION2(atan2) */
/* COMPLEXN_FUNCTION2(gamma_inc) */
/* COMPLEXN_FUNCTION2(beta) */
/* COMPLEXN_FUNCTION2i1(jn) */
/* COMPLEXN_FUNCTION2i1(yn) */
/* COMPLEXN_FUNCTION2(agm) */
/* COMPLEXN_FUNCTION2(hypot) */

/* COMPLEXN_FUNCTION3(fma) */
/* COMPLEXN_FUNCTION3(fms) */

/* COMPLEXN_FUNCTION4(fmma) */
/* COMPLEXN_FUNCTION4(fmms) */

foreign_t complexn_new_value(term_t expr, term_t precision_r, term_t precision_i, term_t value)
{
    complexn *ref;
    int prec_r, prec_i, undefined_prec_r, undefined_prec_i;
    undefined_prec_r = PL_is_variable(precision_r);
    undefined_prec_i = PL_is_variable(precision_i);
    if (!PL_get_integer(precision_r, &prec_r)) {
        if (!undefined_prec_r)
            return FALSE;
        prec_r = 0;
    }
    if (!PL_get_integer(precision_i, &prec_i)) {
        if (!undefined_prec_i)
            return FALSE;
        prec_i = 0;
    }
    switch (PL_term_type(expr)) {
    case PL_VARIABLE:
        /* case PL_NIL: */
        if (!prec_r)
            prec_r = mpfr_get_default_prec();
        if (!prec_i)
            prec_i = mpfr_get_default_prec();
        ref = malloc(sizeof(complexn));
        mpc_init3(*ref, prec_r, prec_i);
        break; // NAN
    case PL_INTEGER:
    {
        mpz_t i;
        mpz_init(i);
        __rtcheck(PL_get_mpz(expr, i));
        if (!prec_r)
            prec_r = MAX(mpfr_get_default_prec(), mpz_sizeinbase(i, 2));
        if (!prec_i)
            prec_i = MAX(mpfr_get_default_prec(), mpz_sizeinbase(i, 2));
        ref = malloc(sizeof(complexn));
        mpc_init3(*ref, prec_r, prec_i);
        mpc_set_z(*ref, i, MPC_RNDNN);
        mpz_clear(i);
        break;
    }
    case PL_TERM:
    {
        mpq_t q;
        mpq_init(q);
        if (!PL_get_mpq(expr, q))
            return FALSE;
        if (!prec_r)
            prec_r = mpfr_get_default_prec();
        if (!prec_i)
            prec_i = mpfr_get_default_prec();
        ref = malloc(sizeof(complexn));
        mpc_init3(*ref, prec_r, prec_i);
        mpc_set_q(*ref, q, MPC_RNDNN);
        mpq_clear(q);
        break;
    }
    case PL_ATOM:
    {
        char *c, *p;
        __rtcheck(PL_get_atom_chars(expr, &c));
        if (!prec_r)
            prec_r = MAX(mpfr_get_default_prec(), strlen(c)*10/3);
        if (!prec_i)
            prec_i = MAX(mpfr_get_default_prec(), strlen(c)*10/3);
        ref = malloc(sizeof(complexn));
        mpc_init3(*ref, prec_r, prec_i);
        mpc_strtoc(*ref, c, &p, 0, MPC_RNDNN);
        if (p==c)
        {
            mpc_clear(*ref);
            free(ref);
            return FALSE;
        }
        break;
    }
    case PL_STRING:
    {
        char *c, *p;
        size_t len;
        __rtcheck(PL_get_string_chars(expr, &c, &len));
        if (!prec_r)
            prec_r = MAX(mpfr_get_default_prec(), strlen(c)*10/3);
        if (!prec_i)
            prec_i = MAX(mpfr_get_default_prec(), strlen(c)*10/3);
        ref = malloc(sizeof(complexn));
        mpc_init3(*ref, prec_r, prec_i);
        mpc_strtoc(*ref, c, 0, 0, MPC_RNDNN);
        if (p==c)
        {
            mpc_clear(*ref);
            free(ref);
            return FALSE;
        }
        break;
    }
    case PL_FLOAT:
    {
        double d;
        __rtcheck(PL_get_float(expr, &d));
        if (!prec_r)
            prec_r = mpfr_get_default_prec();
        if (!prec_i)
            prec_i = mpfr_get_default_prec();
        ref = malloc(sizeof(complexn));
        mpc_init3(*ref, prec_r, prec_i);
        mpc_set_d(*ref, d, MPC_RNDNN);
        break;
    }
    case PL_BLOB:
    {
        void *b;
        PL_blob_t *type;
        PL_get_blob(expr, (void *)&b, NULL, &type);
        if (type == &record_mpc) {
            complexn *v = b;
            mpfr_prec_t vp_r, vp_i;
            mpc_get_prec2(&vp_r, &vp_i, *v);
            if (!prec_r) prec_r = vp_r;
            if (!prec_i) prec_i = vp_i;
            if ((prec_r == vp_r) &&
                (prec_i == vp_i)) {
                if(PL_unify(value, expr)) {
                    if (undefined_prec_r)
                        __rtcheck(PL_unify_integer(precision_r, prec_r));
                    if (undefined_prec_i)
                        __rtcheck(PL_unify_integer(precision_i, prec_i));
                    return TRUE;
                } else
                    return FALSE;
            } else {
                ref = malloc(sizeof(complexn));
                mpc_init3(*ref, prec_r, prec_i);
                mpc_set(*ref, *v, MPC_RNDNN);
            }
        }
        else if (type == record_mpfr) {
            floatn *f = b;
            mpfr_prec_t vp;
            vp = mpfr_get_prec(*f);
            if (!prec_r) prec_r = vp;
            if (!prec_i) prec_i = vp;
            ref = malloc(sizeof(complexn));
            mpc_init3(*ref, prec_r, prec_i);
            mpc_set_fr(*ref, *f, MPC_RNDNN);
        } else
            return FALSE;
        break;
    }
    default:
        return FALSE;
    }
    if (undefined_prec_r)
        __rtcheck(PL_unify_integer(precision_r, prec_r));
    if (undefined_prec_i)
        __rtcheck(PL_unify_integer(precision_i, prec_i));
    return PL_unify_complexn(value, ref);
}

int PL_unify_complexn(term_t t, complexn *fr)
{
    return PL_unify_blob(t, *fr, sizeof(complexn), &record_mpc);
}

int PL_get_complexn(term_t t, complexn **fr)
{
    PL_blob_t *type;
    
    if (PL_get_blob(t, (void **)fr, NULL, &type) && type == &record_mpc)
        return TRUE;
    
    return FALSE;
}
