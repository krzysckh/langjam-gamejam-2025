;;; Commentary:

;;; Calling convention:
;;; ra - working, arg1
;;; rb - working, arg2
;;; rc - working, arg3
;;; rd - working, arg4
;;; re - return
;;; rf
;;; rg - temporary, for macros
;;; rh - temporary, for macros

;;; Code:

constant WIDTH  = 1200
constant HEIGHT = 500

;;; Prelude, has to be defined at 0x0, call main:
def put2(var, v) {
        lit rg, 8
        lit rh, v
        rsh rh, rg
        lit rg, var
        mme rg, rh
        lit rh, 1
        add rg, rh
        lit rh, v
        mme rg, rh
        }

_start: lit rf, stack           ; prepare stack
        put2(stackpt, rf)
        lit ra, main            ; call main
        lit rb, 0
        jiz ra, rb

;;; Variables:

bg:     [0 0]
fg:     [0 0]
vy:     [0 0]
r:      [0 15]
y:      [0 (/ HEIGHT 2)]
x:      [0 (/ WIDTH 2)]

;;; Macros:
def put(var, v) {
        lit rg, var
        lit rh, v
        mme rg, rh
        }

def rput2(var, reg) {
        lit rg, 8
        mov rh, reg
        rsh rh, rg
        lit rg, var
        mme rg, rh
        lit rh, 1
        add rg, rh
        mov rh, reg
        mme rg, rh
        }

def get(var, reg) {
        lit rg, var
        mem reg, rg
        }

def get2(var, reg) {
        lit rg, var
        mem reg, rg
        lit rh, 8
        lsh reg, rh
        lit rh, 1
        add rg, rh
        mem rh, rg
        ior reg, rh
        }

def jmp(x) {
        lit rh, 0
        lit rg, x
        jiz rg, rh
        }

def addn(reg, n) {
        lit rh, n
        add reg, rh
        }

def subn(reg, n) {
        lit rh, n
        sub reg, rh
        }

def dlopen(lib) {
        lit ra, lib
        lod ra, re
        }

def fcall(lib, func) {
        lit rg, lib
        lit rh, func
        exc rh, rg
        }

def M1(a) {
        lit ra, a
        }

def M2(a, b) {
        M1(a)
        lit rb, b
        }

def M3(a, b, c) {
        M2(a, b)
        lit rc, c
        }

def M4(a, b, c, d) {
        M3(a, b, c)
        lit rd, d
        }

def call1(lib, f, a) {
        M1(a)
        fcall(lib, f)
        }

def call2(lib, f, a, b) {
        M2(a, b)
        fcall(lib, f)
        }

def call3(lib, f, a, b, c) {
        M3(a, b, c)
        fcall(lib, f)
        }

def call4(lib, f, a, b, c, d) {
        M4(a, b, c, d)
        fcall(lib, f)
        }

def call0(lib, f) {
        fcall(lib, f)
        }

def make-color(r, g, b, a) {
        call4(0, make_color, r, g, b, a)
        }

def modulo(reg, mod) {
        mov rh, reg
        lit rg, mod
        div reg, rg
        mul reg, rg
        sub rh, reg
        mov reg, rh
        }

def ifz(reg, then, else) {
        ...{                    ; create local scope
           lit rg, test
           lit rh, 0
           jiz rg, rh
a:         ...then
           lit rg, fin
           lit rh, 0
           jiz rg, rh
b:         ...else
           lit rg, fin
           lit rh, 0
           jiz rg, rh
test:      lit rh, a
           jiz rh, reg
           lit rg, b
           lit rh, 0
           jiz rg, rh
           ;; jmp(b)
fin:    }
}

def push(x) {
        get2(stackpt, rf)       ; rf = stackpt
        lit rg, x               ; rg = x
        addn(rf, 1)             ; stackpt++
        mme rf, rg              ; stackpt = x&0xff
        subn(rf, 1)             ; stackpt--
        lit rh, 8               ; rh = 8
        rsh rg, rh              ; rg >>= 8
        mme rf, rg              ; stackpt = rg
        addn(rf, 2)             ; stackpt += 2
        put2(stackpt, rf)       ; stackpt <- stackpt
        }

def pop(reg) {
        get2(stackpt, rf)
        mem reg, rf
        subn(rf, 1)
        mem rg, rf
        subn(rf, 1)
        mem reg, rf
        lit rh, 8
        lsh reg, rh
        ior reg, rg
        put2(stackpt, rf)
        }

def call(ptr) {
        ...{
          push(lmao)
          jmp(ptr)
lmao:   }
        }

def ret() {
        pop(re)
        lit rh, 0
        jiz re, rh
        }

;;; Data declarations:

_Title:             ['H 'e 'l 'l 'o '! 0]
_rl1:               ['r 'l '. 's 'o 0]
InitWindow:         ['W 'I 'n 'i 't 'W 'i 'n 'd 'o 'w 0]
SetTargetFPS:       ['W 'S 'e 't 'T 'a 'r 'g 'e 't 'F 'P 'S 0]
WindowShouldClose:  ['W 'W 'i 'n 'd 'o 'w 'S 'h 'o 'u 'l 'd 'C 'l 'o 's 'e 0]
BeginDrawing:       ['W 'B 'e 'g 'i 'n 'D 'r 'a 'w 'i 'n 'g 0]
EndDrawing:         ['W 'E 'n 'd 'D 'r 'a 'w 'i 'n 'g 0]
ClearBackground:    ['W 'C 'l 'e 'a 'r 'B 'a 'c 'k 'g 'r 'o 'u 'n 'd 0]
DrawCircle:         ['W 'D 'r 'a 'w 'C 'i 'r 'c 'l 'e 0]
SetWindowSize:      ['W 'S 'e 't 'W 'i 'n 'd 'o 'w 'S 'i 'z 'e 0]
GetMousePosition:   ['W 'G 'e 't 'M 'o 'u 's 'e 'P 'o 's 'i 't 'i 'o 'n 0]
GetMouseWheelMove:  ['W 'G 'e 't 'M 'o 'u 's 'e 'W 'h 'e 'e 'l 'M 'o 'v 'e 0]

make_color:         ['m 'a 'k 'e '_ 'c 'o 'l 'o 'r 0]

;;; Functions

fn:     dbg 0, 0
        ret()

main:   dlopen(_rl1)
        call3(0, InitWindow, WIDTH, HEIGHT, _Title)
        call1(0, SetTargetFPS, 30)
        make-color(255, 0, 255, 255)
        put(bg, re)
        make-color(0, 0, 255, 255)
        put(fg, re)
        call2(0, SetWindowSize, WIDTH, HEIGHT)
mainloop:
        call0(0, BeginDrawing)

        get(bg, ra)
        call1(0, ClearBackground, ra)

        get(fg, rd)
        get2(x, ra)
        get2(y, rb)
        get2(r, rc)
        call4(0, DrawCircle, ra, rb, rc, rd)

        call0(0, GetMousePosition)

        ;; modulo(ra, WIDTH)
        ;; modulo(rb, HEIGHT)
        put2(x, re)
        put2(y, rf)

        call0(0, GetMouseWheelMove)

        ifz(re, {}, {
          addn(rc, 5)
          put2(r, rc)
        })

        ifz(rf, {}, {
          subn(rc, 5)
          put2(r, rc)
        })

        call0(0, EndDrawing)

        ;; dbg 0, 0

        call0(0, WindowShouldClose)
        lit rb, mainloop
        jiz rb, re
finish:
        hlt 0, 0

stackpt: [0 0]
stack:
