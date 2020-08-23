sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 2

instr 1
k1 oscil ampdb(p5), (1.0 / p3), 12
a2 oscil (k1 * 0.26), (cpspch(p4) + (1.9e-2 * cpspch(p4))), 13
a3 oscil (k1 * 0.44), (cpspch(p4) + (2.3e-2 * cpspch(p4))), 13
a4 oscil k1, cpspch(p4), 13
outs (((a4 * 0.5) + (a3 * 0.35)) + (a2 * 0.65)), (((a4 * 0.5) + (a3 * 0.65)) + (a2 * 0.35))
endin
