//
//  reader.h
//  Lisper
//
//  Created by Ferhat Ayaz on 10/03/16.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#ifndef reader_h
#define reader_h

#include "exp.h"

#define PERR_OK 0
#define PERR_INV_QUOTE 1
#define PERR_UNK_CHAR 2
#define PERR_FIN 3
#define PERR_INV_QUASIQUOTE 5 /* Invalid quasiquote syntax */
#define PERR_INV_UNQUOTE 6    /* Invalid unquote syntax */
#define PERR_INV_NUMBER 7     /* Invalid number syntax */
#define PERR_TERM 100

expr parse_exp(struct machine *m, const char **sp, int *error);
expr parse_from_file(struct machine *m, const char *fname, int *err);
expr parse_from_string(struct machine *m, const char *buffer, int *err);

#endif /* reader_h */
