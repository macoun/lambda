
LIBOBJ = $(patsubst %.c, %.o, $(LIBSRC))

SRC = $(wildcard *.c)
#SRC = main.c
OBJ = $(patsubst %.c, %.o, $(SRC))

CC = gcc
CFLAGS = -DCOLORED
LDFLAGS = -L. #-lslang

lisper: $(OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) $(LIBS) $(OBJ) -o $@

libslang.a: $(LIBOBJ)
	$(AR) $(ARFLAGS) $@ $^

.PHONY: clean

clean:
	rm *.o lisper > /dev/null 2>&1
