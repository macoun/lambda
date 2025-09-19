#ifndef macros_h
#define macros_h

#include "evaluator.h"

struct macros_expander
{
  struct machine *machine;
  expr *macros; // macro environment stack
};

struct macros_expander *macros_create(struct machine *m);
void macros_destroy(struct macros_expander *expander);

expr macros_preprocess(struct macros_expander *expander, expr source, expr env);

#endif
