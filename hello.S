def jmp(x) {
        lit rh, 0
        mov rg, x
        jiz rg, rh
        }

def add1(reg) {
        lit rh, 1
        add reg, rh
        }

_start: lit ra, main
        lit rb, 0
        jiz ra, rb

hello:  ['H 'e 'l 'l 'o ', ' 'W 'o 'r 'l 'd '! 10 0 ]

main:   lit ra, hello
        gensym(rd)
        mem rb, ra
        lit rc, quit
        jiz rc, rb
        put  1, rb
        add1(ra)
        jmp(rd)

quit:   hlt 0, 0
