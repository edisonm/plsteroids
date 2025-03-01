#include "pl-bid.h"
#include <string.h>
#include <stdio.h>
#include <foreign_interface.h>

/* #define bid64_to_bid64(  a, b) (*(b) = *(a)) */
/* #define bid128_to_bid128(a, b) (*(b) = *(a)) */

#define __cast_to_bid128(ref, v) ({                             \
        if ((PL_new_atom("$bid64")==name)&&(arity==1)) {        \
            BID_UINT64 src;                                     \
            term_t a = PL_new_term_ref();                       \
            __rtcheck(PL_get_arg(1, v, a));                     \
            __rtcheck(PL_get_int64(a, (int64_t *)&src));        \
            BIDECIMAL_CALL1(bid64_to_bid128, ref, src);         \
            break;                                              \
        }})

#define __cast_to_bid64(ref, v) ({                              \
    if ((PL_new_atom("$bid128")==name)&&(arity==2)) {           \
        BID_UINT128 src;                                        \
        term_t a = PL_new_term_ref();                           \
        term_t b = PL_new_term_ref();                           \
        __rtcheck(PL_get_arg(1, v, a));                         \
        __rtcheck(PL_get_arg(2, v, b));                         \
        __rtcheck(PL_get_int64(a, (int64_t *)&(src.w[0])));     \
        __rtcheck(PL_get_int64(b, (int64_t *)&(src.w[1])));     \
        BIDECIMAL_CALL1(bid128_to_bid64, ref, src);             \
        break;                                                  \
    }})

#define GEN_BID_caster(__type, _)                                       \
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
        case PL_TERM:                                                   \
        {   atom_t name;                                                \
            size_t arity;                                               \
            __rtcheck(PL_get_name_arity(v, &name, &arity));             \
            if ((PL_new_atom("$bid"#__type)==name)&&(arity==__type/64)) { \
                return PL_unify(t, v);                                  \
            } else {                                                    \
                __cast_to_bid##__type(ref, v);                          \
            }                                                           \
            return FALSE;                                               \
        }                                                               \
        default:                                                        \
            return FALSE;                                               \
        }                                                               \
        return FI_unify_bid##__type##_t(t, &ref);                       \
    }

#define GEN_BID_pl_2(__type, __func)                                    \
    foreign_t pl_bid##__type##_##__func(term_t res, term_t x) {         \
        bid##__type##_t a;                                              \
        bid##__type##_t b;                                              \
        if (PL_get_bid##__type##_t(x, &a)) {                            \
            BIDECIMAL_CALL1(bid##__type##_##__func, b, a);              \
            return FI_unify_bid##__type##_t(res, &b);                   \
        }                                                               \
        return FALSE;                                                   \
    }

#define GEN_BID_pl_3(__type, __func)                                    \
    foreign_t pl_bid##__type##_##__func(term_t res, term_t x, term_t y) { \
        bid##__type##_t a, b, c;                                        \
        if (FI_get_bid##__type##_t(NULL, x, &a)                         \
            && FI_get_bid##__type##_t(NULL, y, &b)) {                   \
            BIDECIMAL_CALL2(bid##__type##_##__func, c, a, b);           \
            return FI_unify_bid##__type##_t(res, &c);                   \
        }                                                               \
        return FALSE;                                                   \
    }

#define GEN_BID_pn_3(__type, __func)                                    \
    foreign_t pn_bid##__type##_##__func(term_t res, term_t x, term_t y) { \
        bid##__type##_t a, b, c;                                          \
        if (FI_get_bid##__type##_t(NULL, x, &a)                         \
            && FI_get_bid##__type##_t(NULL, y, &b)) {                   \
            BIDECIMAL_CALL2_NORND(bid##__type##_##__func, c, a, b);     \
            return FI_unify_bid##__type##_t(res, &c);                   \
        }                                                               \
        return FALSE;                                                   \
    }

#define GEN_BID_is_2(__type, __func)                                    \
    foreign_t is_bid##__type##_##__func(term_t x, term_t y) {           \
        bid##__type##_t a, b;                                           \
        int c;                                                          \
        if (FI_get_bid##__type##_t(NULL, x, &a)                         \
            && FI_get_bid##__type##_t(NULL, y, &b)) {                   \
            BIDECIMAL_CALL2_NORND(bid##__type##_##__func, c, a, b);     \
            return c;                                                   \
        }                                                               \
        return FALSE;                                                   \
    }

#define GEN_BID_pi_2(__type, __func)                                    \
    foreign_t pi_bid##__type##_##__func(term_t res, term_t x) {         \
        bid##__type##_t a;                                              \
        BID_SINT64 b;                                                   \
        if (FI_get_bid##__type##_t(NULL, x, &a)) {                      \
            BIDECIMAL_CALL1_NORND(bid##__type##_to_int64_##__func, b, a); \
            return PL_unify_int64(res, (int64_t)b);                     \
        }                                                               \
        return FALSE;                                                   \
    }

#define GEN_BID_pt_2(__type, __func)                                    \
    foreign_t pt_bid##__type##_##__func(term_t t, term_t r) {           \
        char s[__type/2];                                               \
        bid##__type##_t v;                                              \
        __rtcheck(PL_get_bid##__type##_t(t, &v));                       \
        BIDECIMAL_CALL1_NORND_RESREF(bid##__type##_to_string, s, v);    \
        __rtcheck(PL_unify_##__func##_chars(r, s));                     \
        return TRUE;                                                    \
    }

int FI_unify_bid64_t(term_t t, bid64_t * const v)
{
    bid64_t src;
    switch (PL_term_type(t)) {
    case PL_VARIABLE:
        return PL_unify_term(t,
                             PL_FUNCTOR_CHARS,
                             "$bid64", 1,
                             PL_INT64,
                             *v);
    case PL_TERM:
        return PL_get_bid64_t(t, &src)
            && (src == *v);
    }
    return FALSE;
}

int FI_unify_bid128_t(term_t t, bid128_t * const v)
{
    bid128_t src;
    switch (PL_term_type(t)) {
    case PL_VARIABLE:
        return PL_unify_term(t,
                             PL_FUNCTOR_CHARS,
                             "$bid128", 2,
                             PL_INT64,
                             v->w[0],
                             PL_INT64,
                             v->w[1]);
    case PL_TERM:
        return PL_get_bid128_t(t, &src)
            && (src.w[0] == v->w[0])
            && (src.w[1] == v->w[1]);
    }
    return FALSE;
}

int PL_get_bid64_t(term_t t, bid64_t *v) {
    atom_t name;
    size_t arity;
    term_t a = PL_new_term_ref();
    return PL_get_name_arity(t, &name, &arity)
        && (arity == 1) && (PL_new_atom("$bid64")==name)
        && PL_get_arg(1, t, a) && PL_get_int64(a, (int64_t *)v);
}

int PL_get_bid128_t(term_t t, bid128_t *v) {
    atom_t name;
    size_t arity;
    term_t a = PL_new_term_ref();
    term_t b = PL_new_term_ref();
    return PL_get_name_arity(t, &name, &arity)
        && (arity == 2) && (PL_new_atom("$bid128")==name)
        && PL_get_arg(1, t, a) && PL_get_int64(a, (int64_t *)&(v->w[0]))
        && PL_get_arg(2, t, b) && PL_get_int64(b, (int64_t *)&(v->w[1]));
}

#define GEN_BID_ALL(__pre, __func) \
    GEN_BID_##__pre( 64, __func) \
    GEN_BID_##__pre(128, __func)

GEN_BID_ALL(caster, _)

#include "pl-bid_auto.h"
