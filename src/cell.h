#ifndef cell_h
#define cell_h

enum cell_type
{
  SINGLE = 0,
  MOVED = 1,
  STRUCT = 2,
  ARRAY = 3,
};

enum expr_type
{
  INTEGER = 2,
  FLOAT = 3,
  SYMBOL = 4,
  STRING = 5,
  PAIR = 6,
  VECTOR = 7,
  PRIMITIVE = 8,
  CUSTOM = 9,
  SYMBOL_SCOPED = 10
};

struct cell
{
  enum cell_type cell_type;
  enum expr_type type;
  union
  {
    void *value;
    char *str;
    struct cell *array;
    int intv;
    long longv;
    double doublev;
    unsigned int uintv;
    unsigned long ulongv;
  };
};

extern const struct cell NIL;
extern const struct cell FALSE;
extern const struct cell TRUE;

#define is_nil(e) (e.type == 0 && e.array == NULL)
#define is_false(e) (e.type == CUSTOM && e.value == FALSE.value)
#define is_real_true(e) (e.type == CUSTOM && e.value == TRUE.value)
#define is_pair(e) ((e).type == PAIR)
#define is_sym(e) ((e).type == SYMBOL)
#define is_str(e) ((e).type == STRING)
#define is_num(e) ((e).type == INTEGER || (e).type == FLOAT)
#define is_prim(e) ((e).type == PRIMITIVE)
#define is_custom(e) ((e).type == CUSTOM)
#define is_vector(e) ((e).type == VECTOR)

#define mk_cell(ct, t, v) ((struct cell){.cell_type = ct, .type = t, v})
#define mk_single_cell(t, v) mk_cell(SINGLE, t, v)

#define mk_int(num) mk_cell(SINGLE, INTEGER, .longv = (long)num)
#define mk_float(num) mk_cell(SINGLE, FLOAT, .doublev = (double)num)
#define mk_num(num) ((num == (long)num) ? mk_int(num) : mk_float(num))
#define mk_str(str) mk_single_cell(STRING, {.value = strdup(str)})
#define mk_sym(str) mk_single_cell(SYMBOL, {.value = strdup(str)})
#define mk_prim(func) mk_single_cell(PRIMITIVE, {.value = func})

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
