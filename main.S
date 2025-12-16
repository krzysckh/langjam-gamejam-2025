;;; Calling convention:
;;; ra - working, arg1
;;; rb - working, arg2
;;; rc - working, arg3
;;; rd - working, arg4
;;; re - return
;;; rf
;;; rg - temporary, for macros
;;; rh - temporary, for macros

;;; Prelude, has to be defined at 0x0, call main:
_start: lit ra, main
        lit rb, 0
        jiz ra, rb

;;; Variables:

bg:     [0 0]
fg:     [0 0]

;;; Macros:
def put(var, v) {
        lit rg, var
        lit rh, v
        mme rg, rh
        }

def get(var, reg) {
        lit rg, var
        mem reg, rg
        }

def jmp(x) {
        lit rh, 0
        lit rg, x
        jiz rg, rh
        }

def add1(reg) {
        lit rh, 1
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

make_color:         ['m 'a 'k 'e '_ 'c 'o 'l 'o 'r 0]

;;; Functions

main:   dlopen(_rl1)
        call3(0, InitWindow, 400, 300, _Title)
        call1(0, SetTargetFPS, 30)
        make-color(255, 0, 255, 255)
        put(bg, re)
        make-color(0, 0, 255, 255)
        put(fg, re)
mainloop:
        call0(0, BeginDrawing)

        get(bg, ra)
        call1(0, ClearBackground, ra)

        get(fg, rd)
        call4(0, DrawCircle, 200, 150, 30, rd)

        call0(0, EndDrawing)

        call0(0, WindowShouldClose)
        lit rb, mainloop
        jiz rb, re
finish:
        hlt 0, 0

;; loop:   lit ra, 97
;;         lit rb, 42
;;         xor(ra, rb)
;;         put 1, re
;;         xor(re, rb)
;;         put 1, re
;;         ;; jmp(loop)
;; quit:   hlt 0, 0
