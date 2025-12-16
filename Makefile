CFLAGS=-Wall -Wextra -std=c89 -g

.SUFFIXES: .c .so

all: rl.so vm
.c.so:
	$(CC) $(CFLAGS) -o $@ -shared $< -lraylib -lm
clean:
	rm -f vm rl.so
