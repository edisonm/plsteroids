#include "pl-floatn.h"
#include <string.h>
// Three figures ~= 10 bits (remember, 2^10=1024)
// equivalent double precision ~= 52 bits (17 decimals)
// equivalent decimal128 precision ~= 110 bits (34 decimals)
// equivalent float128 precision ~= 112 bits (34 decimals)

#define DEFAULT_PRECISION 104
#define DEFAULT_NUMDIGITS  34

#define MAX(a, b) ((a) > (b) ? (a) : (b))

void floatn_to_str(floatn *ref, char **c)
{
    size_t size;
    FILE *stream = open_memstream(c, &size);
    mpfr_out_str(stream, 10, 0, *ref, MPFR_RNDN);
    fclose(stream);
}

static int write_floatn_ref(IOSTREAM *s, atom_t aref, int flags)
{
    mpfr_exp_t e;
    floatn *ref = PL_blob_data(aref, NULL, NULL);
    (void) flags;
    char *c;
    floatn_to_str(ref, &c);
    Sfprintf(s, "%s", c);
    free(c);

    return TRUE;
}

static int release_floatn(atom_t aref)
{
    floatn *ref = PL_blob_data(aref, NULL, NULL);
    char *c;
    floatn_to_str(ref, &c);
    mpfr_clear(*ref);
    free(ref);
    free(c);

    return TRUE;
}

static void aquire_floatn(atom_t aref)
{
    mpfr_t *ref = PL_blob_data(aref, NULL, NULL);
    (void) ref;
}

static PL_blob_t record_mpfr =
{
    PL_BLOB_MAGIC,
    //PL_BLOB_UNIQUE|
    PL_BLOB_NOCOPY,
    "floatn",
    release_floatn,
    NULL,
    write_floatn_ref,
    aquire_floatn
};

int is_floatn(term_t v) {
    void *src;
    PL_blob_t *type;
    return PL_get_blob(v, (void *)&src, NULL, &type)
        && type == &record_mpfr;
}

int is_mpfr_prec_t(term_t v) {
    return PL_is_integer(v);
}

foreign_t floatn_init() {
    // PL_action(PL_GMP_SET_ALLOC_FUNCTIONS, FALSE)
    mp_set_memory_functions(NULL, NULL, NULL);
    return TRUE;
}

#define FLOATN_FUNCTION0(name)                                          \
    foreign_t floatn_##name(term_t p, term_t r)                         \
    {                                                                   \
        floatn *fr;                                                     \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        fr = malloc(sizeof(floatn));                                    \
        if (!PL_get_integer(p, &prec))                                  \
            prec = mpfr_get_default_prec();                             \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, MPFR_RNDN);                                    \
        return PL_unify_floatn(r, fr);                                  \
    }

#define FLOATN_FUNCTION1(name)                                          \
    foreign_t floatn_##name(term_t a, term_t p, term_t r)               \
    {                                                                   \
        floatn *fr;                                                     \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        floatn *ra;                                                     \
        if (!((PL_term_type(a) == PL_BLOB)                              \
              && PL_get_blob(a, (void **)&ra, NULL, &type)              \
              && (type == &record_mpfr)))                               \
            return FALSE;                                               \
        fr = malloc(sizeof(floatn));                                    \
        if (!PL_get_integer(p, &prec))                                  \
            prec = mpfr_get_prec(*ra);                                  \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, *ra, MPFR_RNDN);                               \
        return PL_unify_floatn(r, fr);                                  \
    }

#define FLOATN_FUNCTION1i(name)                                         \
    foreign_t floatn_##name(term_t a, term_t p, term_t r)               \
    {                                                                   \
        floatn *fr;                                                     \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        int ia;                                                         \
        if (!PL_get_integer(a, &ia)) {                                  \
            PL_type_error("integer", a);                                \
            return FALSE;                                               \
        }                                                               \
        fr = malloc(sizeof(floatn));                                    \
        if (!PL_get_integer(p, &prec))                                  \
            prec = mpfr_get_default_prec();                             \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, ia, MPFR_RNDN);                                \
        return PL_unify_floatn(r, fr);                                  \
    }

#define FLOATN_FUNCTION2(name)                                          \
    foreign_t floatn_##name(term_t a, term_t b, term_t p, term_t r)     \
    {                                                                   \
        floatn *fr;                                                     \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        floatn *ra;                                                     \
        if (!((PL_term_type(a) == PL_BLOB)                              \
              && PL_get_blob(a, (void **)&ra, NULL, &type)              \
              && (type == &record_mpfr)))                               \
            return FALSE;                                               \
        floatn *rb;                                                     \
        if (!((PL_term_type(b) == PL_BLOB)                              \
              && PL_get_blob(b, (void **)&rb, NULL, &type)              \
              && (type == &record_mpfr)))                               \
            return FALSE;                                               \
        fr = malloc(sizeof(floatn));                                    \
        if (!PL_get_integer(p, &prec))                                  \
            prec = MAX(mpfr_get_prec(*ra), mpfr_get_prec(*rb));         \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, *ra, *rb, MPFR_RNDN);                          \
        return PL_unify_floatn(r, fr);                                  \
    }

#define FLOATN_FUNCTION2i2(name)                                        \
    foreign_t floatn_##name(term_t a, term_t b, term_t p, term_t r)     \
    {                                                                   \
        floatn *fr;                                                     \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        floatn *ra;                                                     \
        if (!((PL_term_type(a) == PL_BLOB)                              \
              && PL_get_blob(a, (void **)&ra, NULL, &type)              \
              && (type == &record_mpfr)))                               \
            return FALSE;                                               \
        int ib;                                                         \
        if (!PL_get_integer(b, &ib)) {                                  \
            PL_type_error("integer", b);                                \
            return FALSE;                                               \
        }                                                               \
        fr = malloc(sizeof(floatn));                                    \
        if (!PL_get_integer(p, &prec))                                  \
            prec = mpfr_get_prec(*ra);                                  \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, *ra, ib, MPFR_RNDN);                           \
        return PL_unify_floatn(r, fr);                                  \
    }

#define FLOATN_FUNCTION2i1(name)                                        \
    foreign_t floatn_##name(term_t a, term_t b, term_t p, term_t r)     \
    {                                                                   \
        floatn *fr;                                                     \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        int ia;                                                         \
        if (!PL_get_integer(a, &ia)) {                                  \
            PL_type_error("integer", a);                                \
            return FALSE;                                               \
        }                                                               \
        floatn *rb;                                                     \
        if (!((PL_term_type(b) == PL_BLOB)                              \
              && PL_get_blob(b, (void **)&rb, NULL, &type)              \
              && (type == &record_mpfr)))                               \
            return FALSE;                                               \
        fr = malloc(sizeof(floatn));                                    \
        if (!PL_get_integer(p, &prec))                                  \
            prec = mpfr_get_prec(*rb);                                  \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, ia, *rb, MPFR_RNDN);                           \
        return PL_unify_floatn(r, fr);                                  \
    }

#define FLOATN_FUNCTION3(name)                                          \
    foreign_t floatn_##name(term_t a, term_t b, term_t c,               \
                            term_t p, term_t r)                         \
    {                                                                   \
        floatn *fr;                                                     \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        floatn *ra;                                                     \
        if (!((PL_term_type(a) == PL_BLOB)                              \
              && PL_get_blob(a, (void **)&ra, NULL, &type)              \
              && (type == &record_mpfr)))                               \
            return FALSE;                                               \
        floatn *rb;                                                     \
        if (!((PL_term_type(b) == PL_BLOB)                              \
              && PL_get_blob(b, (void **)&rb, NULL, &type)              \
              && (type == &record_mpfr)))                               \
            return FALSE;                                               \
        floatn *rc;                                                     \
        if (!((PL_term_type(b) == PL_BLOB)                              \
              && PL_get_blob(b, (void **)&rc, NULL, &type)              \
              && (type == &record_mpfr)))                               \
            return FALSE;                                               \
        fr = malloc(sizeof(floatn));                                    \
        if (!PL_get_integer(p, &prec))                                  \
            prec = MAX(mpfr_get_prec(*ra),                              \
                       MAX(mpfr_get_prec(*rb),                          \
                           mpfr_get_prec(*rc)));                        \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, *ra, *rb, *rc, MPFR_RNDN);                     \
        return PL_unify_floatn(r, fr);                                  \
    }

#define FLOATN_FUNCTION4(name)                                          \
    foreign_t floatn_##name(term_t a, term_t b, term_t c,               \
                            term_t d, term_t p, term_t r)               \
    {                                                                   \
        floatn *fr;                                                     \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        floatn *ra;                                                     \
        if (!((PL_term_type(a) == PL_BLOB)                              \
              && PL_get_blob(a, (void **)&ra, NULL, &type)              \
              && (type == &record_mpfr)))                               \
            return FALSE;                                               \
        floatn *rb;                                                     \
        if (!((PL_term_type(b) == PL_BLOB)                              \
              && PL_get_blob(b, (void **)&rb, NULL, &type)              \
              && (type == &record_mpfr)))                               \
            return FALSE;                                               \
        floatn *rc;                                                     \
        if (!((PL_term_type(c) == PL_BLOB)                              \
              && PL_get_blob(c, (void **)&rc, NULL, &type)              \
              && (type == &record_mpfr)))                               \
            return FALSE;                                               \
        floatn *rd;                                                     \
        if (!((PL_term_type(d) == PL_BLOB)                              \
              && PL_get_blob(d, (void **)&rd, NULL, &type)              \
              && (type == &record_mpfr)))                               \
            return FALSE;                                               \
        fr = malloc(sizeof(floatn));                                    \
        if (!PL_get_integer(p, &prec))                                  \
            prec = MAX(MAX(mpfr_get_prec(*ra),                          \
                           mpfr_get_prec(*rb)),                         \
                       MAX(mpfr_get_prec(*rc),                          \
                           mpfr_get_prec(*rd)));                        \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, *ra, *rb, *rc, *rd, MPFR_RNDN);                \
        return PL_unify_floatn(r, fr);                                  \
    }

FLOATN_FUNCTION0(const_log2)
FLOATN_FUNCTION0(const_pi)
FLOATN_FUNCTION0(const_euler)
FLOATN_FUNCTION0(const_catalan)

FLOATN_FUNCTION1(sqrt)
FLOATN_FUNCTION1(cbrt)
FLOATN_FUNCTION1(neg)
FLOATN_FUNCTION1(log)
FLOATN_FUNCTION1(log2)
FLOATN_FUNCTION1(log10)
FLOATN_FUNCTION1(log1p)
FLOATN_FUNCTION1(exp)
FLOATN_FUNCTION1(exp2)
FLOATN_FUNCTION1(exp10)
FLOATN_FUNCTION1(expm1)
FLOATN_FUNCTION1(cos)
FLOATN_FUNCTION1(sin)
FLOATN_FUNCTION1(tan)
FLOATN_FUNCTION1(sec)
FLOATN_FUNCTION1(csc)
FLOATN_FUNCTION1(cot)
FLOATN_FUNCTION1(acos)
FLOATN_FUNCTION1(asin)
FLOATN_FUNCTION1(atan)
FLOATN_FUNCTION1(cosh)
FLOATN_FUNCTION1(sinh)
FLOATN_FUNCTION1(tanh)
FLOATN_FUNCTION1(sech)
FLOATN_FUNCTION1(csch)
FLOATN_FUNCTION1(coth)
FLOATN_FUNCTION1(acosh)
FLOATN_FUNCTION1(asinh)
FLOATN_FUNCTION1(atanh)
FLOATN_FUNCTION1i(fac_ui)
FLOATN_FUNCTION1(eint)
FLOATN_FUNCTION1(li2)
FLOATN_FUNCTION1(gamma)
FLOATN_FUNCTION1(lngamma)
FLOATN_FUNCTION1(digamma)
FLOATN_FUNCTION1(zeta)
FLOATN_FUNCTION1(erf)
FLOATN_FUNCTION1(erfc)
FLOATN_FUNCTION1(j0)
FLOATN_FUNCTION1(j1)
FLOATN_FUNCTION1(y0)
FLOATN_FUNCTION1(y1)
FLOATN_FUNCTION1(ai)

FLOATN_FUNCTION2(add)
FLOATN_FUNCTION2(mul)
FLOATN_FUNCTION2(sub)
FLOATN_FUNCTION2(div)
FLOATN_FUNCTION2(pow)
FLOATN_FUNCTION2i2(rootn_ui)
FLOATN_FUNCTION2(atan2)
FLOATN_FUNCTION2(gamma_inc)
FLOATN_FUNCTION2(beta)
FLOATN_FUNCTION2i1(jn)
FLOATN_FUNCTION2i1(yn)
FLOATN_FUNCTION2(agm)
FLOATN_FUNCTION2(hypot)

FLOATN_FUNCTION3(fma)
FLOATN_FUNCTION3(fms)

FLOATN_FUNCTION4(fmma)
FLOATN_FUNCTION4(fmms)

foreign_t floatn_new_value(term_t expr, term_t precision, term_t value)
{
    floatn *ref;
    int prec;
    if (!PL_get_integer(precision, &prec)) {
        if (!PL_is_variable(precision))
            return FALSE;
        prec = 0;
    }
    switch (PL_term_type(expr)) {
    case PL_VARIABLE:
        /* case PL_NIL: */
        if (!prec)
            prec = mpfr_get_default_prec();
        ref = malloc(sizeof(floatn));
        mpfr_init2(*ref, prec);
        break; // NAN
    case PL_INTEGER:
    {
        mpz_t i;
        mpz_init(i);
        PL_get_mpz(expr, i);
        if (!prec)
            prec = MAX(mpfr_get_default_prec(), mpz_sizeinbase(i, 2));
        ref = malloc(sizeof(floatn));
        mpfr_init2(*ref, prec);
        mpfr_set_z(*ref, i, MPFR_RNDN);
        mpz_clear(i);
        break;
    }
    case PL_ATOM:
    {
        char *c, *p;
        PL_get_atom_chars(expr, &c);
        if (!prec)
            prec = MAX(mpfr_get_default_prec(), strlen(c)*10/3);
        ref = malloc(sizeof(floatn));
        mpfr_init2(*ref, prec);
        mpfr_strtofr(*ref, c, &p, 0, MPFR_RNDN);
        if (p==c)
        {
            mpfr_clear(*ref);
            free(ref);
            return FALSE;
        }
        break;
    }
    case PL_STRING:
    {
        char *c, *p;
        size_t len;
        PL_get_string_chars(expr, &c, &len);
        if (!prec)
            prec = MAX(mpfr_get_default_prec(), strlen(c)*10/3);
        ref = malloc(sizeof(floatn));
        mpfr_init2(*ref, prec);
        mpfr_strtofr(*ref, c, 0, 0, MPFR_RNDN);
        if (p==c)
        {
            mpfr_clear(*ref);
            free(ref);
            return FALSE;
        }
        break;
    }
    case PL_FLOAT:
    {
        double d;
        PL_get_float(expr, &d);
        if (!prec)
            prec = mpfr_get_default_prec();
        ref = malloc(sizeof(floatn));
        mpfr_init2(*ref, prec);
        mpfr_set_d(*ref, d, MPFR_RNDN);
        break;
    }
    case PL_BLOB:
    {
        PL_blob_t *type;
        if (!prec)
            prec = mpfr_get_default_prec();
        if (!((PL_get_blob(expr, (void **)&ref, NULL, &type) && (type == &record_mpfr))))
            return FALSE;
        break;
    }
    default:
        return FALSE;
    }
    return PL_unify_floatn(value, ref);
}

int PL_unify_floatn(term_t t, floatn *fr)
{
    return PL_unify_blob(t, *fr, sizeof(floatn), &record_mpfr);
}

int PL_get_floatn(term_t t, floatn **fr)
{
    PL_blob_t *type;
    
    if (PL_get_blob(t, (void **)fr, NULL, &type) && type == &record_mpfr)
        return TRUE;
    
    PL_type_error("floatn", t);
    return FALSE;
}
