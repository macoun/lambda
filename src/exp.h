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


#define set_car(pr, v) do {if (pr.array) pr.array[0] = v;} while (0)
#define set_cdr(pr, v) do {if (pr.array) pr.array[1] = v;} while (0)
#define car(pr) (pr.array?pr.array[0]:NIL)
#define cdr(pr) (pr.array?pr.array[1]:NIL)
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

#define vect_set(vect, idx, item) \
do {if (vect.array) vect.array[idx + 1] = item;} while (0)

#define vect_get(vect, idx) \
((vect.array) ? vect.array[idx + 1] : NIL)

#define vect_size(vect) \
((vect.array) ? vect.array[0].longv : 0)

#define is_nil(e)   (e.type == 0 && e.array == NULL)
#define is_pair(e)  (e.type == PAIR)
#define is_sym(e)   (e.type == SYMBOL)
#define is_str(e) (e.type == STRING)
#define is_num(e) (e.type == INTEGER || e.type == FLOAT)
#define is_prim(e)   (e.type == PRIMITIVE)
#define is_custom(e)   (e.type == CUSTOM)
#define is_vector(e) (e.type == VECTOR)

#define is_eq(a, b) (a->value == b.value)

#define mk_cell(t, v) ((struct cell){t, v})
#define mk_num(num) mk_cell(INTEGER, (void *)num)
#define mk_str(str) mk_cell(STRING, strdup(str))
#define mk_sym(str) mk_cell(SYMBOL, strdup(str))
#define mk_prim(func) mk_cell(PRIMITIVE, func)

#endif /* exp_h */
