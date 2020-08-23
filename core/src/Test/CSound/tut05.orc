sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 2

instr 1
k1 line p6, p3, p7
k2 oscil ampdb(p5), (1.0 / p3), 4
a3 oscil k2, cpspch(p4), 6
outs (a3 * k1), (a3 * (1.0 - k1))
endin
