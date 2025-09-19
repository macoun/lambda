#ifndef pattern_h
#define pattern_h

#include "exp.h"

expr pattern_vars(struct machine *m, expr pattern, expr literals);
expr pattern_depths(struct machine *m, expr pattern, expr literals);

#endif /* pattern_h */
