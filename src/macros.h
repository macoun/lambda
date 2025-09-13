#ifndef macros_h
#define macros_h

#include "evaluator.h"

struct macros_expander
{
  struct machine *machine;
  expr *macros; // List of syntax transformers
};

struct macros_expander *macros_create(struct machine *m);
void macros_destroy(struct macros_expander *expander);

expr macros_collect(struct macros_expander *expander, expr exp);
expr macros_expand(struct macros_expander *expander, expr exp);

// collect + expand
expr macros_preprocess(struct macros_expander *expander, expr source);

#endif
