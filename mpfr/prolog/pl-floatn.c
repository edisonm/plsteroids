#include "pl-floatn.h"
#include <string.h>
#include <foreign_interface.h>

// Three figures ~= 10 bits (remember, 2^10=1024)
// equivalent double precision ~= 52 bits (17 decimals)
// equivalent decimal128 precision ~= 110 bits (34 decimals)
// equivalent float128 precision ~= 112 bits (34 decimals)

#define DEFAULT_PRECISION 104
#define DEFAULT_NUMDIGITS  34

void floatn_to_str(floatn_t *ref, char **c)
{
    size_t size;
    FILE *stream = open_memstream(c, &size);
    mpfr_out_str(stream, 10, 0, *ref, MPFR_RNDN);
    fclose(stream);
}

static int write_floatn_ref(IOSTREAM *s, atom_t aref, int flags)
{
    floatn_t *ref = PL_blob_data(aref, NULL, NULL);
    (void) flags;
    char *c;
    floatn_to_str(ref, &c);
    Sfprintf(s, "%s", c);
    free(c);

    return TRUE;
}

static int release_floatn(atom_t aref)
{
    floatn_t *ref = PL_blob_data(aref, NULL, NULL);
    mpfr_clear(*ref);
    free(ref);

    return TRUE;
}

static void aquire_floatn(atom_t aref)
{
    floatn_t *ref = PL_blob_data(aref, NULL, NULL);
    (void) ref;
}

static PL_blob_t __record_mpfr =
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

PL_blob_t *record_mpfr = &__record_mpfr;

foreign_t is_floatn_t(term_t v) {
    void *src;
    PL_blob_t *type;
    return PL_get_blob(v, (void *)&src, NULL, &type)
        && type == record_mpfr;
}

foreign_t is_mpfr_prec_t(term_t v) {
    return PL_is_integer(v);
}

static void * PL_realloc_mp(void *mem, size_t old_size, size_t new_size) {
    void *p = PL_realloc(mem, new_size);
    return p;
}

static void PL_free_mp(void *mem, size_t size) {
    PL_free(mem);
}

void floatn_init() {
    // PL_action(PL_GMP_SET_ALLOC_FUNCTIONS, FALSE)
    // mp_set_memory_functions(NULL, NULL, NULL);
    // Use prolog alocation functions so that in case of an error, it will be
    // handled in prolog
    mp_set_memory_functions(PL_malloc, PL_realloc_mp, PL_free_mp);
}

#define GEN_FLOATN_pc_2(name)                                           \
    foreign_t pc_floatn_##name(term_t p, term_t r)                      \
    {                                                                   \
        floatn_t *fr;                                                   \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        fr = malloc(sizeof(floatn_t));                                  \
        if (!PL_get_integer(p, &prec))                                  \
            prec = mpfr_get_default_prec();                             \
        mpfr_init2(*fr, prec);                                          \
        mpfr_const_##name(*fr, MPFR_RNDN);                              \
        return PL_unify_floatn_t(r, fr);                                \
    }

#define GEN_FLOATN_pl_3(name)                                           \
    foreign_t pl_floatn_##name(term_t p, term_t r, term_t a)            \
    {                                                                   \
        floatn_t *fr;                                                   \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        floatn_t *ra;                                                   \
        __rtcheck(PL_get_floatn_t(a, &ra));                             \
        fr = malloc(sizeof(floatn_t));                                  \
        if (!PL_get_integer(p, &prec))                                  \
            prec = mpfr_get_prec(*ra);                                  \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, *ra, MPFR_RNDN);                               \
        return PL_unify_floatn_t(r, fr);                                \
    }

#define GEN_FLOATN_pi_3(name)                                           \
    foreign_t pi_floatn_##name(term_t p, term_t r, term_t a)            \
    {                                                                   \
        floatn_t *fr;                                                   \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        int ia;                                                         \
        if (!PL_get_integer(a, &ia)) {                                  \
            PL_type_error("integer", a);                                \
            return FALSE;                                               \
        }                                                               \
        fr = malloc(sizeof(floatn_t));                                  \
        if (!PL_get_integer(p, &prec))                                  \
            prec = mpfr_get_default_prec();                             \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, ia, MPFR_RNDN);                                \
        return PL_unify_floatn_t(r, fr);                                \
    }

#define GEN_FLOATN_pl_4(name)                                           \
    foreign_t pl_floatn_##name(term_t p, term_t r, term_t a, term_t b)  \
    {                                                                   \
        floatn_t *fr;                                                   \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        floatn_t *ra, *rb;                                              \
        __rtcheck(PL_get_floatn_t(a, &ra));                             \
        __rtcheck(PL_get_floatn_t(b, &rb));                             \
        fr = malloc(sizeof(floatn_t));                                  \
        if (!PL_get_integer(p, &prec))                                  \
            prec = MAX(mpfr_get_prec(*ra), mpfr_get_prec(*rb));         \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, *ra, *rb, MPFR_RNDN);                          \
        return PL_unify_floatn_t(r, fr);                                \
    }

#define GEN_FLOATN_ip_4(name)                                           \
    foreign_t ip_floatn_##name(term_t p, term_t r, term_t a, term_t b)  \
    {                                                                   \
        floatn_t *fr;                                                   \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        floatn_t *ra;                                                   \
        __rtcheck(PL_get_floatn_t(a, &ra));                             \
        int ib;                                                         \
        if (!PL_get_integer(b, &ib)) {                                  \
            PL_type_error("integer", b);                                \
            return FALSE;                                               \
        }                                                               \
        fr = malloc(sizeof(floatn_t));                                  \
        if (!PL_get_integer(p, &prec))                                  \
            prec = mpfr_get_prec(*ra);                                  \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, *ra, ib, MPFR_RNDN);                           \
        return PL_unify_floatn_t(r, fr);                                \
    }

#define GEN_FLOATN_pi_4(name)                                           \
    foreign_t pi_floatn_##name(term_t p, term_t r, term_t a, term_t b)  \
    {                                                                   \
        floatn_t *fr;                                                   \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        int ia;                                                         \
        if (!PL_get_integer(a, &ia)) {                                  \
            PL_type_error("integer", a);                                \
            return FALSE;                                               \
        }                                                               \
        floatn_t *rb;                                                   \
        __rtcheck(PL_get_floatn_t(b, &rb));                             \
        fr = malloc(sizeof(floatn_t));                                  \
        if (!PL_get_integer(p, &prec))                                  \
            prec = mpfr_get_prec(*rb);                                  \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, ia, *rb, MPFR_RNDN);                           \
        return PL_unify_floatn_t(r, fr);                                \
    }

#define GEN_FLOATN_pi_2(name)                                           \
    foreign_t pi_floatn_##name(term_t r, term_t a)                      \
    {                                                                   \
        long i;                                                         \
        floatn_t *ra;                                                   \
        __rtcheck(PL_get_floatn_t(a, &ra));                             \
        i = mpfr_##name(*ra, MPFR_RNDN);                                \
        return PL_unify_integer(r, i);                                  \
    }

#define GEN_FLOATN_pl_5(name)                                           \
    foreign_t pl_floatn_##name(term_t p, term_t r,                      \
                               term_t a, term_t b, term_t c)            \
    {                                                                   \
        floatn_t *fr;                                                   \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        floatn_t *ra, *rb, *rc;                                         \
        __rtcheck(PL_get_floatn_t(a, &ra));                             \
        __rtcheck(PL_get_floatn_t(b, &rb));                             \
        __rtcheck(PL_get_floatn_t(c, &rc));                             \
        fr = malloc(sizeof(floatn_t));                                  \
        if (!PL_get_integer(p, &prec))                                  \
            prec = MAX(mpfr_get_prec(*ra),                              \
                       MAX(mpfr_get_prec(*rb),                          \
                           mpfr_get_prec(*rc)));                        \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, *ra, *rb, *rc, MPFR_RNDN);                     \
        return PL_unify_floatn_t(r, fr);                                \
    }

#define GEN_FLOATN_pl_6(name)                                           \
    foreign_t pl_floatn_##name(term_t p, term_t r, term_t a, term_t b,  \
                            term_t c, term_t d)                         \
    {                                                                   \
        floatn_t *fr;                                                   \
        PL_blob_t *type;                                                \
        int prec;                                                       \
        floatn_t *ra, *rb, *rc, *rd;                                    \
        __rtcheck(PL_get_floatn_t(a, &ra));                             \
        __rtcheck(PL_get_floatn_t(b, &rb));                             \
        __rtcheck(PL_get_floatn_t(c, &rc));                             \
        __rtcheck(PL_get_floatn_t(d, &rd));                             \
        fr = malloc(sizeof(floatn_t));                                  \
        if (!PL_get_integer(p, &prec))                                  \
            prec = MAX(MAX(mpfr_get_prec(*ra),                          \
                           mpfr_get_prec(*rb)),                         \
                       MAX(mpfr_get_prec(*rc),                          \
                           mpfr_get_prec(*rd)));                        \
        mpfr_init2(*fr, prec);                                          \
        mpfr_##name(*fr, *ra, *rb, *rc, *rd, MPFR_RNDN);                \
        return PL_unify_floatn_t(r, fr);                                \
    }

#define GEN_FLOATN_is_1(name)                                           \
    foreign_t is_floatn_##name(term_t x)                                \
    {                                                                   \
        floatn_t *rx;                                                   \
        __rtcheck(PL_get_floatn_t(x, &rx));                             \
        return mpfr_##name##_p(*rx);                                    \
    }

#define GEN_FLOATN_is_2(name)                                           \
    foreign_t is_floatn_##name(term_t x, term_t y)                      \
    {                                                                   \
        floatn_t *rx, *ry;                                              \
        __rtcheck(PL_get_floatn_t(x, &rx));                             \
        __rtcheck(PL_get_floatn_t(y, &ry));                             \
        return mpfr_##name##_p(*rx, *ry);                               \
    }

#define GEN_FLOATN_ALL(__pre, __func) GEN_FLOATN_##__pre(__func)

GEN_FLOATN_pi_3(fac_ui)

foreign_t pl_floatn(term_t expr, term_t precision, term_t value)
{
    floatn_t *ref;
    int prec, undefined_prec;
    undefined_prec = PL_is_variable(precision);
    if (!PL_get_integer(precision, &prec)) {
        if (!undefined_prec)
            return FALSE;
        prec = 0;
    }
    switch (PL_term_type(expr)) {
    case PL_VARIABLE:
        /* case PL_NIL: */
        if (!prec)
            prec = mpfr_get_default_prec();
        ref = malloc(sizeof(floatn_t));
        mpfr_init2(*ref, prec);
        break; // NAN
    case PL_INTEGER:
    {
        mpz_t i;
        mpz_init(i);
        __rtcheck(PL_get_mpz(expr, i));
        if (!prec)
            prec = MAX(mpfr_get_default_prec(), mpz_sizeinbase(i, 2));
        ref = malloc(sizeof(floatn_t));
        mpfr_init2(*ref, prec);
        mpfr_set_z(*ref, i, MPFR_RNDN);
        mpz_clear(i);
        break;
    }
    case PL_TERM:
    {
        mpq_t q;
        mpq_init(q);
        if (!PL_get_mpq(expr, q))
            return FALSE;
        if (!prec)
            prec = mpfr_get_default_prec();
        ref = malloc(sizeof(floatn_t));
        mpfr_init2(*ref, prec);
        mpfr_set_q(*ref, q, MPFR_RNDN);
        mpq_clear(q);
        break;
    }
    case PL_ATOM:
    {
        char *c, *p;
        __rtcheck(PL_get_atom_chars(expr, &c));
        if (!prec)
            prec = MAX(mpfr_get_default_prec(), strlen(c)*10/3);
        ref = malloc(sizeof(floatn_t));
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
        __rtcheck(PL_get_string_chars(expr, &c, &len));
        if (!prec)
            prec = MAX(mpfr_get_default_prec(), strlen(c)*10/3);
        ref = malloc(sizeof(floatn_t));
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
        __rtcheck(PL_get_float(expr, &d));
        if (!prec)
            prec = mpfr_get_default_prec();
        ref = malloc(sizeof(floatn_t));
        mpfr_init2(*ref, prec);
        mpfr_set_d(*ref, d, MPFR_RNDN);
        break;
    }
    case PL_BLOB:
    {
        PL_blob_t *type;
        floatn_t *v;
        int vp;
        __rtcheck(PL_get_floatn_t(expr, &v));
        vp = mpfr_get_prec(*v);
        if (!prec) prec = vp;
        if (prec == vp) {
            if (undefined_prec)
                __rtcheck(PL_unify_integer(precision, prec));
            return PL_unify(value, expr);
        } else {
            ref = malloc(sizeof(floatn_t));
            mpfr_init2(*ref, prec);
            mpfr_set(*ref, *v, MPFR_RNDN);
        }
        break;
    }
    default:
        return FALSE;
    }
    if (undefined_prec)
        __rtcheck(PL_unify_integer(precision, prec));
    return PL_unify_floatn_t(value, ref);
}

int PL_unify_floatn_t(term_t t, floatn_t *fr)
{
    return PL_unify_blob(t, fr, sizeof(floatn_t), record_mpfr);
}

int PL_get_floatn_t(term_t t, floatn_t **fr)
{
    PL_blob_t *type;
    
    if (PL_get_blob(t, (void **)fr, NULL, &type) && type == record_mpfr)
        return TRUE;
    
    /* PL_type_error("floatn", t); */
    return FALSE;
}

#include "pl-floatn_auto.h"
