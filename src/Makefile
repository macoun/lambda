
LIBOBJ = $(patsubst %.c, %.o, $(LIBSRC))

SRC = $(wildcard *.c)
#SRC = main.c
OBJ = $(patsubst %.c, %.o, $(SRC))

CC = gcc
CFLAGS = -DCOLORED
LDFLAGS = -L. #-lslang

lambda: $(OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) $(LIBS) $(OBJ) -o $@

liblambda.a: $(LIBOBJ)
	$(AR) $(ARFLAGS) $@ $^

.PHONY: clean

clean:
	rm *.o lambda > /dev/null 2>&1
