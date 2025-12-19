;;; Commentary:

;;; Calling convention:
;;; ra - working, arg1
;;; rb - working, arg2
;;; rc - working, arg3
;;; rd - working, arg4
;;; re - return
;;; rf - return / ??
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

_start:
        lit rf, stack           ; prepare stack
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

def ecall1(lib, f, a) {
        M1(a)
        fcall(lib, f)
        }

def ecall2(lib, f, a, b) {
        M2(a, b)
        fcall(lib, f)
        }

def ecall3(lib, f, a, b, c) {
        M3(a, b, c)
        fcall(lib, f)
        }

def ecall4(lib, f, a, b, c, d) {
        M4(a, b, c, d)
        fcall(lib, f)
        }

def ecall0(lib, f) {
        fcall(lib, f)
        }

def make-color(r, g, b, a) {
        ecall4(0, make_color, r, g, b, a)
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

def ifl(a, b, then, else) {
        lss b, a
        ifz(b, {...then}, {...else})
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

;; def push(x) {
;;         lit rh, x
;;         lit rg, 8
;;         rsh rh, rg
;;         get2(stackpt, rg)
;;         mme rg, rh
;;         lit rh, x
;;         addn(rg, 1)
;;         mme rg, rh
;;         addn(rg, 1)
;;         put2(stackpt, rg)
;;         dbg 1,1
;;         }

;;; i hate myself
def pop(reg) {
        ...{
        lit rh, lol
        lit rg, 0
        jiz rh, rg
_F1:    [0]
_F2:    [0]
lol:
        put2(_F1, rf)
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
        get2(_F1, rf)
        }
        }

def call(ptr) {
        ...{
          push(lmao)
          push(ra)
          push(rb)
          push(rc)
          push(rd)
          jmp(ptr)
lmao:   }
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

def ret() {
        ...{
        pop(rd)
        pop(rc)
        pop(rb)
        pop(ra)
        lit rh, lol
        lit rg, 0
        jiz rh, rg
_A1:    [0]
_A2:    [0]
lol:
        put2(_A1, ra)
        pop(ra)
        mov rg, ra
        lit ra, 0
        lit rh, _A1
        mem ra, rh
        lit rh, 8
        lsh ra, 8
        lit rh, _A2
        mem rh, rh
        ior ra, rh
        lit rh, 0
        jiz rg, rh
        }
        }

;;; Data declarations:

_Title:             ['H 'e 'l 'l 'o '! 0]
_rl1:               ['r 'l '. 's 'o 0]
InitWindow:         ['W 'I 'n 'i 't 'W 'i 'n 'd 'o 'w 0] ;
SetTargetFPS:       ['W 'S 'e 't 'T 'a 'r 'g 'e 't 'F 'P 'S 0]
WindowShouldClose:  ['W 'W 'i 'n 'd 'o 'w 'S 'h 'o 'u 'l 'd 'C 'l 'o 's 'e 0]
BeginDrawing:       ['W 'B 'e 'g 'i 'n 'D 'r 'a 'w 'i 'n 'g 0]
EndDrawing:         ['W 'E 'n 'd 'D 'r 'a 'w 'i 'n 'g 0]
ClearBackground:    ['W 'C 'l 'e 'a 'r 'B 'a 'c 'k 'g 'r 'o 'u 'n 'd 0]
DrawCircle:         ['W 'D 'r 'a 'w 'C 'i 'r 'c 'l 'e 0]
SetWindowSize:      ['W 'S 'e 't 'W 'i 'n 'd 'o 'w 'S 'i 'z 'e 0]
GetMousePosition:   ['W 'G 'e 't 'M 'o 'u 's 'e 'P 'o 's 'i 't 'i 'o 'n 0]
GetMouseWheelMove:  ['W 'G 'e 't 'M 'o 'u 's 'e 'W 'h 'e 'e 'l 'M 'o 'v 'e 0]
Vec2MoveTowards:    ['W 'V 'e 'c 't 'o 'r '2 'M 'o 'v 'e 'T 'o 'w 'a 'r 'd 's 0]

make_color:         ['m 'a 'k 'e '_ 'c 'o 'l 'o 'r 0]

;;; Functions

initialize-raylib:
        dlopen(_rl1)
        ret()

init-window:
        ecall3(0, InitWindow, WIDTH, HEIGHT, _Title)
        ecall1(0, SetTargetFPS, 30)
        ret()

draw-player:
        get(fg, rd)
        get2(x, ra)
        get2(y, rb)
        get2(r, rc)
        ecall4(0, DrawCircle, ra, rb, rc, rd)
        ret()

move-towards:                   ; (x, y, xt, yt) -> (x', y')
        ecall4(0, Vec2MoveTowards, ra, rb, rc, rd)
        dbg 1,1
        ret()

update-player:
        ecall0(0, GetMousePosition) ; re = x', rf = y'
        get2(x, ra)                 ; ra = x
        get2(y, rb)                 ; rb = y
        mov rc, re
        mov rd, rf
        call(move-towards)
        dbg 1,1
        put2(x, re)
        put2(y, rf)
        ret()

main:
        lit ra, 67
        lit rb, 42
        lit rc, 69
        lit rd, 420
        call(initialize-raylib)
        call(init-window)

        make-color(255, 0, 255, 255)
        put(bg, re)
        make-color(0, 0, 255, 255)
        put(fg, re)
        ecall2(0, SetWindowSize, WIDTH, HEIGHT)

mainloop:
        ecall0(0, WindowShouldClose)
        ifz(re, {}, { jmp(finish) })
        ecall0(0, BeginDrawing)

        get(bg, ra)
        ecall1(0, ClearBackground, ra)

        call(draw-player)
        call(update-player)

        ;; ecall0(0, GetMouseWheelMove)

        ;; ifz(re, {}, {
        ;;   addn(rc, 5)
        ;;   put2(r, rc)
        ;; })

        ;; ifz(rf, {}, {
        ;;   subn(rc, 5)
        ;;   put2(r, rc)
        ;; })

        ecall0(0, EndDrawing)
        jmp(mainloop)
finish:
        hlt 0, 0

stackpt: [0 0]
stack:
