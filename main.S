;;; Calling convention:
;;; ra - working, arg1
;;; rb - working, arg2
;;; rc - working, arg3
;;; rd - working, arg4
;;; re - return
;;; rf
;;; rg - temporary, for macros
;;; rh - temporary, for macros

constant WIDTH  = 400
constant HEIGHT = 300

;;; Prelude, has to be defined at 0x0, call main:
_start: lit ra, main
        lit rb, 0
        jiz ra, rb

;;; Variables:

bg:     [0 0]
fg:     [0 0]
vy:     [0 0]
y:      [0 (/ HEIGHT 2)]
x:      [0 (/ WIDTH 2)]

;;; Macros:
def put(var, v) {
        lit rg, var
        lit rh, v
        mme rg, rh
        }

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

;; def xor(a, b) {
;;         M(a, b)
;;         fcall1(_xor)
;;         }

;; def ior(a, b) {
;;         M(a, b)
;;         fcall1(_ior)
;;         }

;; def lsh(a, b) {
;;         M(a, b)
;;         fcall1(_lsh)
;;         }

;; def rsh(a, b) {
;;         M(a, b)
;;         fcall1(_rsh)
;;         }

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

make_color:         ['m 'a 'k 'e '_ 'c 'o 'l 'o 'r 0]

;;; Functions

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
        call4(0, DrawCircle, ra, rb, 30, rd)
        addn(rb, 10)
        addn(ra, 10)
        modulo(ra, WIDTH)
        modulo(rb, HEIGHT)
        put2(x, ra)
        put2(y, rb)

        call0(0, EndDrawing)

        call0(0, WindowShouldClose)
        lit rb, mainloop
        jiz rb, re
finish:
        hlt 0, 0
