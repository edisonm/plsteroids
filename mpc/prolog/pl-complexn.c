#include "pl-floatn.h"
#include "pl-complexn.h"
#include <string.h>
#include <dlfcn.h>
#include <foreign_interface.h>

static int write_complexn_ref(IOSTREAM *stream, atom_t aref, int flags)
{
    complexn_t *ref = PL_blob_data(aref, NULL, NULL);

    floatn_t *imag = &mpc_imagref(*ref);
    if (mpfr_zero_p(*imag)) {
        return floatn_out_str(stream, &mpc_realref(*ref));
    } else {
        return (Sfprintf(stream, "(")>0)
            && floatn_out_str(stream, &mpc_realref(*ref))
            && Sfprintf(stream, ",")
            && floatn_out_str(stream, imag)
            && (Sfprintf(stream, ")")>0) ;
    }
}

static int release_complexn(atom_t aref)
{
    complexn_t *ref = PL_blob_data(aref, NULL, NULL);
    mpc_clear(*ref);
    free(ref);

    return TRUE;
}

static void aquire_complexn(atom_t aref)
{
    complexn_t *ref = PL_blob_data(aref, NULL, NULL);
    (void) ref;
}

static PL_blob_t __record_mpc =
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

PL_blob_t *record_mpc = &__record_mpc;

foreign_t is_complexn_t(term_t v) {
    void *src;
    PL_blob_t *type;
    return PL_get_blob(v, (void *)&src, NULL, &type)
        && type == record_mpc;
}

/*
#define GEN_COMPLEXN_pc_3(name)                                         \
    foreign_t pc_complexn_##name(term_t p_r, term_t p_i, term_t r)      \
    {                                                                   \
        complexn_t *cx;                                                 \
        PL_blob_t *type;                                                \
        mpfr_prec_t prec_r, prec_i;                                     \
        cx = malloc(sizeof(complexn_t));                                \
        if (!PL_get_long(p_r, &prec_r))                                 \
            prec_r = mpfr_get_default_prec();                           \
        if (!PL_get_long(p_i, &prec_i))                                 \
            prec_i = mpfr_get_default_prec();                           \
        mpc_init3(*cx, prec_r, prec_i);                                 \
        mpc_##name(*cx, MPC_RNDNN);                                     \
        return PL_unify_complexn(r, cx);                                \
    }
*/

#define GEN_COMPLEXN_pc_3(name)                                         \
    foreign_t pc_complexn_##name(term_t p_r, term_t p_i, term_t r)      \
    {                                                                   \
        complexn_t *cx;                                                 \
        PL_blob_t *type;                                                \
        mpfr_prec_t prec_r, prec_i;                                     \
        cx = malloc(sizeof(complexn_t));                                \
        if (!PL_get_long(p_r, &prec_r))                                 \
            prec_r = mpfr_get_default_prec();                           \
        if (!PL_get_long(p_i, &prec_i))                                 \
            prec_i = mpfr_get_default_prec();                           \
        mpc_init3(*cx, prec_r, prec_i);                                 \
        mpc_##name(*cx);                                                \
        return PL_unify_complexn_t(r, cx);                              \
    }

#define GEN_COMPLEXN_pi_3(name)                                         \
    foreign_t pi_complexn_##name(term_t r, term_t x, term_t y)          \
    {                                                                   \
        complexn_t *cx, *cy;                                            \
        __rtcheck(PL_get_complexn_t(x, &cx));                           \
        __rtcheck(PL_get_complexn_t(y, &cy));                           \
        return PL_unify_integer(r, mpc_##name(*cx, *cy));               \
    }

// COMPLEXN_FUNCTION1
#define GEN_COMPLEXN_pl_4(name)                                         \
    foreign_t pl_complexn_##name(term_t p_r, term_t p_i, term_t r, term_t a) \
    {                                                                   \
        complexn_t *cx;                                                 \
        PL_blob_t *type;                                                \
        mpfr_prec_t prec_ar, prec_r, prec_i, prec_ai;                   \
        complexn_t *ra;                                                 \
        __rtcheck(PL_get_complexn_t(a, &ra));                           \
        cx = malloc(sizeof(complexn_t));                                \
        mpc_get_prec2(&prec_ar, &prec_ai, *ra);                         \
        if (!PL_get_long(p_r, &prec_r))                                 \
            prec_r = prec_ar;                                           \
        if (!PL_get_long(p_i, &prec_i))                                 \
            prec_i = prec_ai;                                           \
        mpc_init3(*cx, prec_r, prec_i);                                 \
        mpc_##name(*cx, *ra, MPC_RNDNN);                                \
        return PL_unify_complexn_t(r, cx);                              \
    }

// FLOATN_FUNCTION1
#define GEN_COMPLEXN_pf_4(name)                                         \
    foreign_t pf_complexn_##name(term_t p_r, term_t p_i, term_t r, term_t a) \
    {                                                                   \
        complexn_t *cx;                                                 \
        floatn_t fr;                                                    \
        PL_blob_t *type;                                                \
        mpfr_prec_t prec_r, prec_i, prec_ar, prec_ai;                   \
        complexn_t *ra;                                                 \
        __rtcheck(PL_get_complexn_t(a, &ra));                           \
        cx = malloc(sizeof(complexn_t));                                \
        mpc_get_prec2(&prec_ar, &prec_ai, *ra);                         \
        if (!PL_get_long(p_r, &prec_r))                                 \
            prec_r = prec_ar;                                           \
        if (!PL_get_long(p_i, &prec_i))                                 \
            prec_i = prec_ai;                                           \
        mpc_init3(*cx, prec_r, prec_i);                                 \
        mpfr_init2(fr, MAX(prec_r, prec_i));                            \
        mpc_##name(fr, *ra, MPFR_RNDN);                                 \
        mpc_set_fr(*cx, fr, MPC_RNDNN);                                 \
        mpfr_clear(fr);                                                 \
        return PL_unify_complexn_t(r, cx);                              \
    }

// COMPLEXN_FUNCTION2
#define GEN_COMPLEXN_pl_5(name)                                         \
    foreign_t pl_complexn_##name(term_t p_r, term_t p_i, term_t r, term_t a, term_t b) \
    {                                                                   \
        complexn_t *cx;                                                 \
        PL_blob_t *type;                                                \
        mpfr_prec_t prec_r, prec_i, prec_ar, prec_ai, prec_br, prec_bi; \
        complexn_t *ra, *rb;                                            \
        __rtcheck(PL_get_complexn_t(a, &ra));                           \
        __rtcheck(PL_get_complexn_t(b, &rb));                           \
        cx = malloc(sizeof(complexn_t));                                \
        mpc_get_prec2(&prec_ar, &prec_ai, *ra);                         \
        mpc_get_prec2(&prec_br, &prec_bi, *rb);                         \
        if (!PL_get_long(p_r, &prec_r))                                 \
            prec_r = MAX(prec_ar, prec_br);                             \
        if (!PL_get_long(p_i, &prec_i))                                 \
            prec_i = MAX(prec_ai, prec_bi);                             \
        mpc_init3(*cx, prec_r, prec_i);                                 \
        mpc_##name(*cx, *ra, *rb, MPC_RNDNN);                           \
        return PL_unify_complexn_t(r, cx);                              \
    }

// COMPLEXN_FUNCTION2f
#define GEN_COMPLEXN_pf_5(name)                                         \
    foreign_t pf_complexn_##name(term_t a, term_t b, term_t p_r, term_t p_i, term_t r) \
    {                                                                   \
        complexn_t *cx;                                                 \
        PL_blob_t *type;                                                \
        mpfr_prec_t prec_r, prec_i;                                     \
        floatn_t *ra, *rb;                                              \
        __rtcheck(PL_get_floatn_t(a, &ra));                             \
        __rtcheck(PL_get_floatn_t(b, &rb));                             \
        cx = malloc(sizeof(complexn_t));                                \
        if (!PL_get_long(p_r, &prec_r))                                 \
            prec_r = mpfr_get_prec(*ra);                                \
        if (!PL_get_long(p_i, &prec_i))                                 \
            prec_i = mpfr_get_prec(*rb);                                \
        mpc_init3(*cx, prec_r, prec_i);                                 \
        mpc_##name(*cx, *ra, *rb, MPC_RNDNN);                           \
        return PL_unify_complexn_t(r, cx);                              \
    }

/* not available, some to implement in prolog
GEN_COMPLEXN_pl_4(cbrt)
GEN_COMPLEXN_pl_4(log2)
GEN_COMPLEXN_pl_4(log1p)
GEN_COMPLEXN_pl_4(exp2)
GEN_COMPLEXN_pl_4(expm1)
GEN_COMPLEXN_pl_4(sec)
GEN_COMPLEXN_pl_4(csc)
GEN_COMPLEXN_pl_4(cot)
GEN_COMPLEXN_pl_4(exp10)
GEN_COMPLEXN_pl_4(sech)
GEN_COMPLEXN_pl_4(csch)
GEN_COMPLEXN_pl_4(coth)
GEN_COMPLEXN_pl_4(eint)
GEN_COMPLEXN_pl_4(li2)
GEN_COMPLEXN_pl_4(gamma)
GEN_COMPLEXN_pl_4(lngamma)
GEN_COMPLEXN_pl_4(digamma)
GEN_COMPLEXN_pl_4(zeta)
GEN_COMPLEXN_pl_4(erf)
GEN_COMPLEXN_pl_4(erfc)
GEN_COMPLEXN_pl_4(j0)
GEN_COMPLEXN_pl_4(j1)
GEN_COMPLEXN_pl_4(y0)
GEN_COMPLEXN_pl_4(y1)
GEN_COMPLEXN_pl_4(ai)
*/

/* COMPLEXN_FUNCTION2i2(rootn_ui) */
/* GEN_COMPLEXN_pl_5(atan2) */
/* GEN_COMPLEXN_pl_5(gamma_inc) */
/* GEN_COMPLEXN_pl_5(beta) */
/* COMPLEXN_FUNCTION2i1(jn) */
/* COMPLEXN_FUNCTION2i1(yn) */
/* GEN_COMPLEXN_pl_5(agm) */
/* GEN_COMPLEXN_pl_5(hypot) */

/* COMPLEXN_FUNCTION3(fma) */
/* COMPLEXN_FUNCTION3(fms) */

/* COMPLEXN_FUNCTION4(fmma) */
/* COMPLEXN_FUNCTION4(fmms) */

#define GEN_COMPLEXN_ALL(__pre, __func) GEN_COMPLEXN_##__pre(__func)

foreign_t pl_complexn(term_t expr, term_t precision_r, term_t precision_i, term_t value)
{
    complexn_t *ref;
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
        ref = malloc(sizeof(complexn_t));
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
        ref = malloc(sizeof(complexn_t));
        mpc_init3(*ref, prec_r, prec_i);
        mpc_set_z(*ref, i, MPC_RNDNN);
        mpz_clear(i);
        break;
    }
    case PL_TERM:
    {
        if (!prec_r)
            prec_r = mpfr_get_default_prec();
        if (!prec_i)
            prec_i = mpfr_get_default_prec();
        
        mpq_t q;
        mpq_init(q);
        if (PL_get_mpq(expr, q)) {
            ref = malloc(sizeof(complexn_t));
            mpc_init3(*ref, prec_r, prec_i);
            mpc_set_q(*ref, q, MPC_RNDNN);
            mpq_clear(q);
            break;
        } else {
            mpq_clear(q);
            return FALSE;
        }
        mpfr_t *r;
        if (PL_get_floatn_t(expr, &r)) {
            if (!prec_r)
                prec_r = mpfr_get_default_prec();
            if (!prec_i)
                prec_i = mpfr_get_default_prec();
            ref = malloc(sizeof(complexn_t));
            mpc_init3(*ref, prec_r, prec_i);
            mpc_set_fr(*ref, *r, MPC_RNDNN);
            break;
        } else {
            return FALSE;
        }
    }
    case PL_ATOM:
    {
        char *c, *p;
        __rtcheck(PL_get_atom_chars(expr, &c));
        if (!prec_r)
            prec_r = MAX(mpfr_get_default_prec(), strlen(c)*10/3);
        if (!prec_i)
            prec_i = MAX(mpfr_get_default_prec(), strlen(c)*10/3);
        ref = malloc(sizeof(complexn_t));
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
        ref = malloc(sizeof(complexn_t));
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
        ref = malloc(sizeof(complexn_t));
        mpc_init3(*ref, prec_r, prec_i);
        mpc_set_d(*ref, d, MPC_RNDNN);
        break;
    }
    case PL_BLOB:
    {
        void *b;
        PL_blob_t *type;
        PL_get_blob(expr, (void *)&b, NULL, &type);
        if (type == record_mpc) {
            complexn_t *v = b;
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
                ref = malloc(sizeof(complexn_t));
                mpc_init3(*ref, prec_r, prec_i);
                mpc_set(*ref, *v, MPC_RNDNN);
            }
        }
        else if (type == record_mpfr) {
            floatn_t *f = b;
            mpfr_prec_t vp;
            vp = mpfr_get_prec(*f);
            if (!prec_r) prec_r = vp;
            if (!prec_i) prec_i = vp;
            ref = malloc(sizeof(complexn_t));
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
    return PL_unify_complexn_t(value, ref);
}

int PL_unify_complexn_t(term_t t, complexn_t *fr)
{
    return PL_unify_blob(t, fr, sizeof(complexn_t), record_mpc);
}

int PL_get_complexn_t(term_t t, complexn_t **fr)
{
    PL_blob_t *type;
    
    if (PL_get_blob(t, (void **)fr, NULL, &type) && type == record_mpc)
        return TRUE;
    
    return FALSE;
}

#include "pl-complexn_auto.h"
