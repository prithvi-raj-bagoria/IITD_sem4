# Compiler and flags
CC = gcc
CFLAGS = -Wall -Wextra -std=c99
OBJS = main.o sheet.o parser.o

# Build the executable "sheet"
sheet: $(OBJS)
	$(CC) $(CFLAGS) -o sheet $(OBJS) -lm

# Build object files
main.o: main.c sheet.h parser.h
	$(CC) $(CFLAGS) -c main.c

sheet.o: sheet.c sheet.h parser.h
	$(CC) $(CFLAGS) -c sheet.c

parser.o: parser.c parser.h sheet.h
	$(CC) $(CFLAGS) -c parser.c

clean:
	rm -f *.o sheet
