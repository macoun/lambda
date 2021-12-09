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

expr op_is_null(expr args);
expr op_is_number(expr args);
expr op_is_string(expr args);
expr op_is_quoted(expr args);
expr op_is_pair(expr args);
expr op_is_symbol(expr args);

expr op_println(expr args);
expr op_print(expr args);

expr op_cdr(expr args);
expr op_car(expr args);
expr op_cons(expr args);
expr op_list(expr args);

expr op_eq(expr args);
expr op_lt(expr args);
expr op_gt(expr args);

expr op_add(expr args);
expr op_sub(expr args);
expr op_mul(expr args);

expr op_vector_create(expr args);
expr op_vector_size(expr args);
expr op_vector_get(expr args);
expr op_vector_set(expr args);

#endif /* primitives_h */
