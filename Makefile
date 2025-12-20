CFLAGS=-Wall -Wextra -std=gnu99 -g

.SUFFIXES: .c .so .S .bin

all: rl.so vm main.bin
.S.bin:
	ol -r as.scm $< $@
.c.so:
	$(CC) $(CFLAGS) -o $@ -shared $< -lraylib -lm
clean:
	rm -f vm rl.so main.bin
