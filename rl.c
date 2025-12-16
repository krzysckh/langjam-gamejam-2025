#include <raylib.h>

#include "vm.h"

static unsigned nc = 0;
static Color colors[512] = {0};

#define VOID(f) void W ## f (__attribute__((unused))State *s) { f(); }

#define SOF(s, A) ((void*)((s)->data+A(s)))

void
WInitWindow(State *s)
{
  InitWindow(RA(s), RB(s), SOF(s, RC));
}

void
WSetTargetFPS(State *s)
{
  SetTargetFPS(RA(s));
}

void
WWindowShouldClose(State *s)
{
  RE(s) = WindowShouldClose();
}

void
make_color(State *s)
{
  colors[nc] = (Color) {RA(s), RB(s), RC(s), RD(s)};
  RE(s) = nc++;
}

void
WClearBackground(State *s)
{
  ClearBackground(colors[RA(s)]);
}

void
WDrawCircle(State *s)
{
  DrawCircle(RA(s), RB(s), RC(s), colors[RD(s)]);
}

VOID(BeginDrawing);
VOID(EndDrawing);
