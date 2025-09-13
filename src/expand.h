#ifndef expand_h
#define expand_h

#include "exp.h"

expr expand_template(struct machine *m, expr template, int depth, expr bindings, expr patternvars, expr idxstack);

#endif /* expand_h */
