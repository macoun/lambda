//
//  primitives.h
//  Lisper
//
//  Created by Ferhat Ayaz on 19/03/16.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#ifndef primitives_h
#define primitives_h

#include "exp.h"

expr op_is_null(struct machine *m, expr args);
expr op_is_number(struct machine *m, expr args);
expr op_is_string(struct machine *m, expr args);
expr op_is_quoted(struct machine *m, expr args);
expr op_is_pair(struct machine *m, expr args);
expr op_is_symbol(struct machine *m, expr args);

expr op_is_zero(struct machine *m, expr args);
expr op_is_positive(struct machine *m, expr args);
expr op_is_negative(struct machine *m, expr args);
expr op_is_odd(struct machine *m, expr args);
expr op_is_even(struct machine *m, expr args);

expr op_println(struct machine *m, expr args);
expr op_print(struct machine *m, expr args);

expr op_cdr(struct machine *m, expr args);
expr op_car(struct machine *m, expr args);
expr op_cons(struct machine *m, expr args);
expr op_list(struct machine *m, expr args);

expr op_eq(struct machine *m, expr args);
expr op_lt(struct machine *m, expr args);
expr op_gt(struct machine *m, expr args);

expr op_add(struct machine *m, expr args);
expr op_sub(struct machine *m, expr args);
expr op_mul(struct machine *m, expr args);

expr op_vector_create(struct machine *m, expr args);
expr op_vector_size(struct machine *m, expr args);
expr op_vector_get(struct machine *m, expr args);
expr op_vector_set(struct machine *m, expr args);

expr primitives_env(struct machine *ev);

#endif /* primitives_h */
