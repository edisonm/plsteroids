#include "pl-bid.h"
#include <string.h>
#include <stdio.h>
#include <foreign_interface.h>

#define GEN_bid_write_ref(__type)                                       \
    static int write_bid##__type##_ref(IOSTREAM *stream,                \
                                       atom_t aref, int flags)          \
    {                                                                   \
        char s[__type/2];                                               \
        bid##__type##_t *ref = PL_blob_data(aref, NULL, NULL);          \
        BIDECIMAL_CALL1_NORND_RESREF(bid##__type##_to_string, s, *ref); \
        if (Sfputs(s, stream) == EOF)                                   \
            return FALSE;                                               \
        return TRUE;                                                    \
    }

#define GEN_bid_release(__type)                                 \
    static int release_bid##__type(atom_t aref)                 \
    {                                                           \
        bid##__type##_t *ref = PL_blob_data(aref, NULL, NULL);  \
        PL_free(ref);                                           \
        return TRUE;                                            \
    }

#define GEN_bid_aquire(__type)                                  \
    static void aquire_bid##__type(atom_t aref)                 \
    {                                                           \
        bid##__type##_t *ref = PL_blob_data(aref, NULL, NULL);  \
        (void) ref;                                             \
    }

#define GEN_bid___record(__type)                \
    static PL_blob_t __record_bid##__type##_t = \
    {                                           \
        PL_BLOB_MAGIC,                          \
        PL_BLOB_NOCOPY,                         \
        "bid"#__type,                           \
        release_bid##__type,                    \
        NULL,                                   \
        write_bid##__type##_ref,                \
        aquire_bid##__type                      \
    };

#define GEN_bid_record(__type)                                  \
    PL_blob_t *record_bid##__type##_t = &__record_bid##__type##_t;

#define GEN_bid_is(__type)                                       \
    foreign_t is_bid##__type##_t(term_t v) {                     \
        void *src;                                               \
        PL_blob_t *type;                                         \
        return PL_get_blob(v, (void *)&src, NULL, &type)         \
            && type == record_bid##__type##_t;                   \
    }

#define GEN_bid_caster(__type)                                          \
    foreign_t pl_bid##__type(term_t v, term_t t) {                      \
        BID_UINT##__type ref;                                           \
        switch (PL_term_type(v)) {                                      \
        case PL_VARIABLE: break;                                        \
        case PL_INTEGER:                                                \
        {                                                               \
            long long i;                                                \
            int result = PL_get_int64(v, (int64_t *)&i);                \
            if (!result) return FALSE;                                  \
            BIDECIMAL_CALL1(bid##__type##_from_int64, ref, i);          \
            break;                                                      \
        }                                                               \
        case PL_ATOM:                                                   \
        {   char *c;                                                    \
            char *s;                                                    \
            __rtcheck(PL_get_atom_chars(v, &c));                        \
            ref = bid_strtod##__type(c, &s);                            \
            if (*s!='\0') return FALSE;                                 \
            break;                                                      \
        }                                                               \
        case PL_STRING:                                                 \
        {   char *c;                                                    \
            size_t len;                                                 \
            char *s;                                                    \
            __rtcheck(PL_get_string_chars(v, &c, &len));                \
            ref = bid_strtod##__type(c, &s);                            \
            if (*s!='\0') return FALSE;                                 \
            break;                                                      \
        }                                                               \
        case PL_FLOAT:                                                  \
        {   double d;                                                   \
            __rtcheck(PL_get_float(v, &d));                             \
            BIDECIMAL_CALL1(binary64_to_bid##__type, ref, d);           \
            break;                                                      \
        }                                                               \
        case PL_BLOB:                                                   \
        {                                                               \
            PL_blob_t *type;                                            \
            if (is_bid##__type##_t(v)) {                                \
                return PL_unify(t, v);                                  \
            } else                                                      \
                return FALSE;                                           \
        }                                                               \
        default:                                                        \
            return FALSE;                                               \
        }                                                               \
        bid##__type##_t *res = PL_malloc(sizeof(bid##__type##_t));      \
        *res = ref;                                                     \
        return FI_unify_bid##__type##_t(t, res);                        \
    }

#define GEN_bid_pl_2(__type, __func)                                    \
    foreign_t pl_bid##__type##_##__func(term_t res, term_t x) {         \
        bid##__type##_t *a;                                             \
        if (PL_get_bid##__type##_t(x, &a)) {                            \
            bid##__type##_t *b = PL_malloc(sizeof(bid##__type##_t));    \
            BIDECIMAL_CALL1(bid##__type##_##__func, *b, *a);            \
            return FI_unify_bid##__type##_t(res, b);                    \
        }                                                               \
        return FALSE;                                                   \
    }

#define GEN_bid_pl_3(__type, __func)                                    \
    foreign_t pl_bid##__type##_##__func(term_t res, term_t x, term_t y) { \
        bid##__type##_t *a, *b;                                         \
        if (FI_get_bid##__type##_t(NULL, x, &a)                         \
            && FI_get_bid##__type##_t(NULL, y, &b)) {                   \
            bid##__type##_t *c = PL_malloc(sizeof(bid##__type##_t));    \
            BIDECIMAL_CALL2(bid##__type##_##__func, *c, *a, *b);        \
            return FI_unify_bid##__type##_t(res, c);                    \
        }                                                               \
        return FALSE;                                                   \
    }

#define GEN_bid_pn_3(__type, __func)                                    \
    foreign_t pn_bid##__type##_##__func(term_t res, term_t x, term_t y) { \
        bid##__type##_t *a, *b;                                         \
        if (FI_get_bid##__type##_t(NULL, x, &a)                         \
            && FI_get_bid##__type##_t(NULL, y, &b)) {                   \
            bid##__type##_t *c = PL_malloc(sizeof(bid##__type##_t));    \
            BIDECIMAL_CALL2_NORND(bid##__type##_##__func, *c, *a, *b);  \
            return FI_unify_bid##__type##_t(res, c);                    \
        }                                                               \
        return FALSE;                                                   \
    }

#define GEN_bid_is_2(__type, __func)                                    \
    foreign_t is_bid##__type##_##__func(term_t x, term_t y) {           \
        bid##__type##_t *a, *b;                                         \
        int c;                                                          \
        if (FI_get_bid##__type##_t(NULL, x, &a)                         \
            && FI_get_bid##__type##_t(NULL, y, &b)) {                   \
            BIDECIMAL_CALL2_NORND(bid##__type##_##__func, c, *a, *b);   \
            return c;                                                   \
        }                                                               \
        return FALSE;                                                   \
    }

#define GEN_bid_pi_2(__type, __func)                                    \
    foreign_t pi_bid##__type##_##__func(term_t res, term_t x) {         \
        bid##__type##_t *a;                                             \
        if (FI_get_bid##__type##_t(NULL, x, &a)) {                      \
            BID_SINT64 b;                                               \
            BIDECIMAL_CALL1_NORND(bid##__type##_to_int64_##__func, b, *a); \
            return PL_unify_int64(res, (int64_t)b);                     \
        }                                                               \
        return FALSE;                                                   \
    }

#define GEN_bid_pt_2(__type, __func)                                    \
    foreign_t pt_bid##__type##_##__func(term_t t, term_t r) {           \
        char s[__type/2];                                               \
        bid##__type##_t *v;                                             \
        __rtcheck(PL_get_bid##__type##_t(t, &v));                       \
        BIDECIMAL_CALL1_NORND_RESREF(bid##__type##_to_string, s, *v);   \
        __rtcheck(PL_unify_##__func##_chars(r, s));                     \
        return TRUE;                                                    \
    }

#define GEN_bid_pl_unify(__type)                                        \
    int PL_unify_bid##__type##_t(term_t t, bid##__type##_t *fr) {       \
        return PL_unify_blob(t, fr, sizeof(bid##__type##_t),            \
                             record_bid##__type##_t);                   \
    }

#define GEN_bid_pl_get(__type)                                          \
    int PL_get_bid##__type##_t(term_t t, bid##__type##_t **v) {         \
        PL_blob_t *type;                                                \
        if (PL_get_blob(t, (void **)v, NULL, &type)                     \
            && type == record_bid##__type##_t)                          \
            return TRUE;                                                \
        return FALSE;                                                   \
    }

#define GEN_bid_ALL(__pre, __func) \
    GEN_bid_##__pre( 64, __func) \
    GEN_bid_##__pre(128, __func)

#define GEN_bid_ALL_1(__pre) \
    GEN_bid_##__pre( 64)     \
    GEN_bid_##__pre(128)

GEN_bid_ALL_1(caster)
GEN_bid_ALL_1(write_ref)
GEN_bid_ALL_1(release)
GEN_bid_ALL_1(aquire)
GEN_bid_ALL_1(__record)
GEN_bid_ALL_1(record)
GEN_bid_ALL_1(is)
GEN_bid_ALL_1(pl_unify)
GEN_bid_ALL_1(pl_get)

#include "pl-bid_auto.h"
