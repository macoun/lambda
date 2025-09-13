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

expr env_parent(expr env);
expr env_frame(expr env);
expr env_extend(struct machine *m, expr vars, expr vals, expr env);
expr env_define_variable(struct machine *m, expr var, expr val, expr env);

#endif /* env_h */
