//
//  exp.h
//  Lisper
//
//  Created by Ferhat Ayaz on 13/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#ifndef exp_h
#define exp_h

#include <stdio.h>
#include "mem.h"
#include "machine.h"

typedef struct cell expr;

expr cons(struct machine *m, expr a, expr b);

expr list(struct machine *m, expr first, ...);
expr listn(struct machine *m, int n, ...);
expr list_append(struct machine *m, expr l, expr e);
expr list_append_x(struct machine *m, expr l, expr e);
expr list_ref(expr l, expr n);
expr list_reverse(struct machine *m, expr l);
long list_length(expr l);
expr list_tail(expr l);
int is_list(expr e);

expr memq(expr memb, expr lst);
expr assoq(expr key, expr alist);

expr vector(struct machine *m, long length, expr *exps);

bool is_equal(expr a, expr b); // should dispatch by data type
bool is_eq(expr a, expr b);    // pointer equality, value equality for symbols
// bool is_eqv(expr a, expr b);   // pointer equality for non-scalars, value equality for scalars

#define set_car(pr, v)       \
    do                       \
    {                        \
        if (pr.array)        \
            pr.array[0] = v; \
    } while (0)
#define set_cdr(pr, v)       \
    do                       \
    {                        \
        if (pr.array)        \
            pr.array[1] = v; \
    } while (0)

#define car(pr) ((is_pair(pr) && pr.array) ? pr.array[0] : NIL) // !is_pair() is actually a contract violation
#define cdr(pr) ((is_pair(pr) && pr.array) ? pr.array[1] : NIL)
#define caar(pr) car(car(pr))
#define cadr(pr) car(cdr(pr))
#define cddr(pr) cdr(cdr(pr))
#define cdar(pr) cdr(car(pr))
#define caadr(pr) car(cadr(pr))
#define caddr(pr) car(cddr(pr))
#define cdadr(pr) cdr(cadr(pr))
#define cdddr(pr) cdr(cddr(pr))
#define cadddr(pr) car(cdddr(pr))
#define cddddr(pr) cdr(cdddr(pr))

#endif /* exp_h */
