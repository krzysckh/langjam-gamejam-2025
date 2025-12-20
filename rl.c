#include <raylib.h>
#include <raymath.h>
#include <stdlib.h>

#include "vm.h"

static unsigned nc = 0;
static Color colors[512] = {0};

#define VOID(f)     void W ## f (__attribute__((unused))State *s) { f(); }
#define VDO(f, ...) void W ## f (__attribute__((unused))State *s) { f(__VA_ARGS__); }

#define SOF(s, A) ((void*)((s)->data+A(s)))

void
WVector2MoveTowards(State *s)
{
  Vector2 v = Vector2MoveTowards((Vector2){RA(s), RB(s)},
                                 (Vector2){RC(s), RD(s)},
                                 5);
  RE(s) = (uint16_t)floor(v.x);
  RF(s) = (uint16_t)floor(v.y);
}

void
WWindowShouldClose(State *s)
{
  RE(s) = WindowShouldClose();
}

void
WIsKeyPressed(State *s)
{
  RE(s) = IsKeyPressed(RA(s));
}

void
WGetMouseWheelMove(State *s)
{
  float f = GetMouseWheelMove();
  RE(s) = RF(s) = 0;

  if (f < 0.f) RF(s) = 1;
  if (f > 0.f) RE(s) = 1;
}

void
WGetMousePosition(State *s)
{
  RE(s) = GetMouseX();
  RF(s) = GetMouseY();
}

void
make_color(State *s)
{
  colors[nc] = (Color) {RA(s), RB(s), RC(s), RD(s)};
  RE(s) = nc++;
}

void
getrandom(State *s)
{
  RE(s) = arc4random();
}

VDO(ClearBackground, colors[RA(s)])
VDO(DrawCircle, RA(s), RB(s), RC(s), colors[RD(s)])
VDO(SetWindowSize, RA(s), RB(s))
VDO(InitWindow, RA(s), RB(s), SOF(s, RC));
VDO(SetTargetFPS, RA(s))
VDO(DrawRectanglePro, (Rectangle){RA(s), RB(s), 10, 10}, (Vector2){0, 0}, RD(s), colors[RE(s)])
VOID(BeginDrawing);
VOID(EndDrawing);
