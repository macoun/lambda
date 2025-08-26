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
#define PERR_TERM 100

expr parse_exp(struct machine *m, char **sp, int *error);

#endif /* reader_h */
