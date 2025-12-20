#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <inttypes.h>
#include <assert.h>
#include <string.h>
#include <err.h>
#include <dlfcn.h>
#include <ctype.h>

#include "vm.h"

/*
  BYTECODE:

  * normal instruction:

  0x__ 0xAB
    ^^   ^^
    \____||_ opcode
         |\_ 2nd arg
         \_  1st arg


  * literal instruction

  0x00 0xA_ 0xBB 0xCC
    ^^   ^^   ^^   ^^
    \____||___|____|____ opcode
         |\___|____|____ unused
         \____|____|____ REG
              \____|____ data1
                   \____ data2, where new value of REG is (data1<<8)|data2

 */

#define LIT 0x00 /* LITeral value */
#define MOV 0x01 /* MOV a b: MOVe data from b to a */
#define NOT 0x02 /* NOT a b: a = ~b */
#define PUT 0x03 /* PUT a b: print char from reg b to fd a */
#define HLT 0x04 /* HLT _ _: solve halting problem */
#define JIZ 0x05 /* JIZ a b: jump to A if B is 0 */
#define MEM 0x06 /* MEM a b: get BYTE from memory pointed by B into A */
#define MME 0x07 /* MEM a b: put BYTE to memory at A from B */
#define ADD 0x08 /* ADD a b: a = a+b */
#define SUB 0x09 /* SUB a b: a = a-b */
#define MUL 0x0a /* MUL a b: a = a*b */
#define DIV 0x0b /* DIV a b: a = a/b */
#define AND 0x0c /* AND a b: a = a&b */
#define XOR 0x0d /* XOR a b: a = a^b */
#define IOR 0x0e /* IOR a b: a = a|b */
#define LSH 0x0f /* LSH a b: a = a<<b */
#define RSH 0x10 /* RSH a b: a = a>>b */
#define LSS 0x11 /* LSS a b: a = a<b ? 1 : 0 */
#define LOD 0x12 /* LOD a b: load library with name pointed by A, save library pointer into B */
#define EXC 0x13 /* EXC a b: execute function with name pointed by A from library poitner B. gets passed current State */
#define DBG 0xff /* DBG a b: print current state */

#define I(s, ...) do { __VA_ARGS__ ; } while (0); (s)->pc += 2; break;
#define B(O, o) case O: I(s, R1(s) o R2(s))

void
print_state(State *s)
{
  int i;
  printf("[%d] Registers: ", s->pc);
  for (i = 0; i < NREG; ++i)
    printf("r%c=%d, ", 'a'+i, s->reg[i]); /*  */
  putchar('\n');
}

void
print_region(State *s)
{
  uint16_t begin = R1(s), sz = R2(s), i;
  printf("Memory dump from %04x to %04x:\n", begin, begin+sz);
  for (i = begin; i < begin+sz; ++i)
    printf("%04x: %02x (%d)\t[%c]\n", i, s->data[i], s->data[i], isprint(s->data[i]) ? s->data[i] : ' ');
}

static uint16_t libmax = 0;
static void *libs[0xffff] = {0};

int
main(int argc, char **argv)
{
  State *s = malloc(sizeof(State));
  memset(s, 0, sizeof(State));

  assert(argc == 2);

  fread(s->data, 1, 0xffff, fopen(argv[1], "r"));
  memset(s->reg, 0, sizeof(*s->reg)*NREG);
  s->pc = s->haltp = 0;

  while (!s->haltp) {
    /* print_state(s); */
    switch (s->data[s->pc]) {
    case LIT: s->reg[s->data[s->pc+1]%NREG] = (s->data[s->pc+2]<<8)|s->data[s->pc+3]; s->pc+=4; break;
    case MOV: I(s, R1(s) = R2(s))
    case NOT: I(s, R1(s) = ~R2(s))
    case PUT: I(s, write(D1(s), &R2(s), 1); fsync(D1(s)))
    case HLT: s->haltp = 1; break;
    case JIZ: s->pc = R2(s) == 0 ? R1(s) : s->pc+2; break;
    case MEM: I(s, R1(s) = s->data[R2(s)])
    case MME: I(s, s->data[R1(s)] = R2(s)&0xff)
       B(ADD, +=)
       B(SUB, -=)
       B(MUL, *=)
       B(DIV, /=)
       B(AND, &=)
       B(XOR, ^=)
       B(IOR, |=)
       B(LSH, <<=)
       B(RSH, >>=)
    case LSS: I(s, R1(s) = R1(s) < R2(s))
    case LOD: I(s, R2(s) = libmax; libs[libmax++] = dlopen((void*)(s->data+R1(s)), RTLD_NOW);
                if (!libs[R2(s)]) errx(1, "couldn't open %s: %s", (char*)(s->data+R1(s)), dlerror()))
    case EXC: I(s, void (*f)(State*) = dlsym(libs[R2(s)], (void*)s->data+R1(s));
                if (f) f(s); else errx(1, "undefined in %d: %s", R2(s), (char*)(void*)s->data+R1(s)))
    case DBG: I(s, if (D1(s) == 0 && D2(s) == 0) print_state(s); else print_region(s))
    default:
      errx(1, "unknown opcode %d", s->data[s->pc]);
    }
  }

  return 0;
}
