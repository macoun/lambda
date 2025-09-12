#ifndef cell_h
#define cell_h

enum cell_type
{
  MOVED = 1,
  INTEGER = 2,
  FLOAT = 3,
  SYMBOL = 4,
  STRING = 5,
  PAIR = 6,
  VECTOR = 7,
  PRIMITIVE = 8,
  CUSTOM = 9
};

struct cell
{
  enum cell_type type;
  union
  {
    void *value;
    char *str;
    struct cell *array;
    int intv;
    long longv;
    unsigned int uintv;
    unsigned long ulongv;
  };
};

extern const struct cell NIL;
extern const struct cell FALSE;
extern const struct cell TRUE;

#define is_nil(e) (e.type == 0 && e.array == NULL)
#define is_false(e) (e.type == CUSTOM && e.array == FALSE.array)
#define is_real_true(e) (e.type == CUSTOM && e.array == TRUE.array)
#define is_pair(e) ((e).type == PAIR)
#define is_sym(e) ((e).type == SYMBOL)
#define is_str(e) ((e).type == STRING)
#define is_num(e) ((e).type == INTEGER || (e).type == FLOAT)
#define is_prim(e) ((e).type == PRIMITIVE)
#define is_custom(e) ((e).type == CUSTOM)
#define is_vector(e) ((e).type == VECTOR)
#define is_compound(e) (is_pair(e) || is_vector(e))

#define mk_cell(t, v) ((struct cell){t, {v}})
#define mk_num(num) mk_cell(INTEGER, (void *)num)
#define mk_str(str) mk_cell(STRING, strdup(str))
#define mk_sym(str) mk_cell(SYMBOL, strdup(str))
#define mk_prim(func) mk_cell(PRIMITIVE, func)

#define has_moved(e) ((e).type == MOVED)

#define vect_set(vect, idx, item) \
  do                              \
  {                               \
    if (vect.array)               \
      vect.array[idx + 1] = item; \
  } while (0)

#define vect_get(vect, idx) \
  ((vect.array) ? vect.array[idx + 1] : NIL)

#define vect_size(vect) \
  ((vect.array) ? vect.array[0].longv : 0)

#endif /* cell_h */
