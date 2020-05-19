#
# Makefile for uscheme-trace
#

RESULT   = uscheme-trace
OBJDIR   = .
SOURCES  = arith.c ast-code.c env.c error.c eval.c evaldef.c\
           lex.c linestream.c list-code.c loc.c name.c\
           overflow.c par-code.c parse.c prim.c print.c\
           printbuf.c printfuns.c scheme-tests.c scheme.c\
           tableparsing.c tests.c unicode.c value-code.c\
           value.c xdefstream.c
HEADERS  = all.h prim.h
OBJ      = $(OBJDIR)/uscheme-
OBJECTS  = $(OBJ)arith.o $(OBJ)ast-code.o $(OBJ)env.o\
           $(OBJ)error.o $(OBJ)eval.o $(OBJ)evaldef.o\
           $(OBJ)lex.o $(OBJ)linestream.o $(OBJ)list-code.o\
           $(OBJ)loc.o $(OBJ)name.o $(OBJ)overflow.o\
           $(OBJ)par-code.o $(OBJ)parse.o $(OBJ)prim.o\
           $(OBJ)print.o $(OBJ)printbuf.o $(OBJ)printfuns.o\
           $(OBJ)scheme-tests.o $(OBJ)scheme.o\
           $(OBJ)tableparsing.o $(OBJ)tests.o $(OBJ)unicode.o\
           $(OBJ)value-code.o $(OBJ)value.o\
           $(OBJ)xdefstream.o

CC = gcc -std=c99 -pedantic -Wall -Werror -Wextra -Wno-overlength-strings
CFLAGS = -g 
LDFLAGS = -g
CPPFLAGS = -I.
RM = rm -f 

.SUFFIXES:
.PHONY: all

all: $(RESULT)

$(RESULT): $(OBJECTS)
	$(CC) -o $@ $(LDFLAGS) $(OBJECTS)

clean:
	$(RM) $(RESULT) $(OBJDIR)/*.o *.core core *~

$(OBJ)env.o: env.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)env.o -c env.c
$(OBJ)eval.o: eval.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)eval.o -c eval.c
$(OBJ)printfuns.o: printfuns.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)printfuns.o -c printfuns.c
$(OBJ)parse.o: parse.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)parse.o -c parse.c
$(OBJ)error.o: error.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)error.o -c error.c
$(OBJ)lex.o: lex.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)lex.o -c lex.c
$(OBJ)linestream.o: linestream.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)linestream.o -c linestream.c
$(OBJ)name.o: name.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)name.o -c name.c
$(OBJ)overflow.o: overflow.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)overflow.o -c overflow.c
$(OBJ)arith.o: arith.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)arith.o -c arith.c
$(OBJ)print.o: print.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)print.o -c print.c
$(OBJ)printbuf.o: printbuf.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)printbuf.o -c printbuf.c
$(OBJ)tableparsing.o: tableparsing.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)tableparsing.o -c tableparsing.c
$(OBJ)tests.o: tests.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)tests.o -c tests.c
$(OBJ)unicode.o: unicode.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)unicode.o -c unicode.c
$(OBJ)xdefstream.o: xdefstream.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)xdefstream.o -c xdefstream.c
$(OBJ)evaldef.o: evaldef.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)evaldef.o -c evaldef.c
$(OBJ)loc.o: loc.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)loc.o -c loc.c
$(OBJ)prim.o: prim.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)prim.o -c prim.c
$(OBJ)scheme.o: scheme.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)scheme.o -c scheme.c
$(OBJ)scheme-tests.o: scheme-tests.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)scheme-tests.o -c scheme-tests.c
$(OBJ)value.o: value.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)value.o -c value.c
$(OBJ)value-code.o: value-code.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)value-code.o -c value-code.c
$(OBJ)ast-code.o: ast-code.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)ast-code.o -c ast-code.c
$(OBJ)par-code.o: par-code.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)par-code.o -c par-code.c
$(OBJ)list-code.o: list-code.c $(HEADERS)
	$(CC) $(CFLAGS) $(CPPFLAGS) -o $(OBJ)list-code.o -c list-code.c
