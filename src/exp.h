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

typedef struct cell expr;

expr eval(expr exp);
expr cons(expr a, expr b);

expr list(expr first, ...);
expr listn(int n, ...);
expr list_append(expr l, expr e);
long list_length(expr l);
expr list_tail(expr l);
int is_list(expr e);

expr vector(long length, expr *exps);

int is_equal(expr a, expr b); // should dispatch by data type

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
#define car(pr) (pr.array ? pr.array[0] : NIL)
#define cdr(pr) (pr.array ? pr.array[1] : NIL)
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
