
LIBOBJ = $(patsubst %.c, %.o, $(LIBSRC))

SRC = $(wildcard *.c)
#SRC = main.c
OBJDIR = ../obj
OBJ = $(patsubst %.c, $(OBJDIR)/%.o, $(SRC))

CC = gcc
# CC = clang
CFLAGS = -std=c99 -Wall -DCOLORED
LDFLAGS = -L. #-lslang

$(OBJDIR)/%.o : %.c
	$(CC) $(CFLAGS) -c $< -o $@

lambda: $(OBJ)
	$(CC) $(CFLAGS) $(LDFLAGS) $(LIBS) $(OBJ) -o $@

liblambda.a: $(LIBOBJ)
	$(AR) $(ARFLAGS) $@ $^

.PHONY: clean

clean:
	rm $(OBJ) lambda > /dev/null 2>&1
