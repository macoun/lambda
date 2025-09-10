#ifndef match_h
#define match_h
#include "exp.h"

expr match_pattern(struct machine *m, expr pattern, expr input, expr literals);

#endif /* match_h */
