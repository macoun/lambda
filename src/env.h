//
//  env.h
//  Lisper
//
//  Created by Ferhat Ayaz on 14/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#ifndef env_h
#define env_h

#include "exp.h"
#include "machine.h"

expr *env_lookup(expr var, expr env);
expr *frame_lookup(expr frame, expr var);

expr env_parent(expr env);
expr env_frame(expr env);
expr env_extend(struct machine *m, expr vars, expr vals, expr env);
expr env_define_variable(struct machine *m, expr var, expr val, expr env);

expr match_pattern(struct machine *m, expr pattern, expr exp, expr bindings);
expr expand_template(struct machine *m, expr template, int depth, expr bindings, expr patternvars, expr idxstack);
expr pattern_vars(struct machine *m, expr pattern, int depth, expr literals, expr bindings);

#endif /* env_h */
