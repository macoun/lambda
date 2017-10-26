//
//  exp.c
//  Lisper
//
//  Created by Ferhat Ayaz on 13/03/2016.
//  Copyright © 2016 Ferhat Ayaz. All rights reserved.
//

#include "exp.h"
#include "mem.h"

#include <stdarg.h>
#include <assert.h>

expr cons(expr a, expr b)
{
  expr pair;
  
  pair.type = PAIR;
  push(a);
  push(b);
  pair.array = mem_new(2);
  pop(b);
  pop(a);
  
  pair.array[0] = a;
  pair.array[1] = b;
  return pair;
}

expr list(expr first, ...)
{
  va_list ap;
  expr head, tail, cur;
  
  head = cons(first, NIL);
  tail = head;
  
  va_start(ap, first);
  for (cur = va_arg(ap, expr); !is_nil(cur);
       cur = va_arg(ap, expr))
  {
    set_cdr(tail, cons(cur, NIL));
    tail = cdr(tail);
  }
  va_end(ap);

  return head;
}

expr listn(int n, ...)
{
  int i;
  expr l = NIL, cur;
  
  va_list ap;
  
  va_start(ap, n);
  for (i = 0; i < n; i++)
  {
    cur = va_arg(ap, expr);
    push(cur);
  }
  va_end(ap);
  
  for (i = 0; i < n; i++)
  {
    pop(cur);
    l = cons(cur, l);
  }
  
  return l;
}

expr list_tail(expr l)
{
  while (is_pair(l) && is_pair(cdr(l)))
    l = cdr(l);
  return l;
}

int is_list(expr e)
{
  return is_nil(cdr(list_tail(e)));
}

long list_length(expr l)
{
  long len;
  
  len = 0;
  while (is_pair(l))
  {
    l = cdr(l);
    len++;
  }
  return len;
}

expr list_append(expr l, expr e)
{
  expr tmp, tail;
  
  if (is_nil(l))
    return cons(e, NIL);
  
  push(l);
  tmp = cons(e, NIL);
  pop(l);
  tail = list_tail(l);
  set_cdr(tail, tmp);
  return l;
}

expr vector(long length, expr *exps)
{
  expr vect;
  int i;
  
  vect.type = VECTOR;
  vect.array = mem_new(length + 1);
  vect.array[0] = mk_num(length);
  
  for (i = 0; i < length; i++)
    vect.array[i + 1] = exps ? exps[i] : NIL;
  
  return vect;
}

