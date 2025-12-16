#include <inttypes.h>

#define NREG 8

typedef struct State
{
  uint8_t haltp, data[0xffff];
  uint16_t reg[NREG], pc;
} State;


#define D1(s) (((s)->data[(s)->pc+1]&0xf0)>>4)
#define D2(s) ((s)->data[(s)->pc+1]&0xf)

#define R1(s) ((s)->reg[D1(s)])
#define R2(s) ((s)->reg[D2(s)])

#define RA(s) ((s)->reg[0])
#define RB(s) ((s)->reg[1])
#define RC(s) ((s)->reg[2])
#define RD(s) ((s)->reg[3])
#define RE(s) ((s)->reg[4])
#define RF(s) ((s)->reg[5])
#define RG(s) ((s)->reg[6])
#define RH(s) ((s)->reg[7])
