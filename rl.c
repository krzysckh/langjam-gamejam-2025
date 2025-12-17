#include <raylib.h>

#include "vm.h"

static unsigned nc = 0;
static Color colors[512] = {0};

#define VOID(f)     void W ## f (__attribute__((unused))State *s) { f(); }
#define VDO(f, ...) void W ## f (__attribute__((unused))State *s) { f(__VA_ARGS__); }

#define SOF(s, A) ((void*)((s)->data+A(s)))

void
WWindowShouldClose(State *s)
{
  RE(s) = WindowShouldClose();
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

VDO(ClearBackground, colors[RA(s)])
VDO(DrawCircle, RA(s), RB(s), RC(s), colors[RD(s)])
VDO(SetWindowSize, RA(s), RB(s))
VDO(InitWindow, RA(s), RB(s), SOF(s, RC));
VDO(SetTargetFPS, RA(s))

VOID(BeginDrawing);
VOID(EndDrawing);
