sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 2

instr 1
a1 oscil (1.0 / 10.0), 2.0, 1
k2 line 0.0, p3, p7
a3 oscil ((cpspch(p4) * p6) * k2), (cpspch(p4) * p6), 1
k4 oscil 1.0, (1.0 / p3), 4
a5 oscil (ampdb(p5) * k4), (cpspch(p4) + a3), 1
a6 vdelay a5, (0.5 + a1), (1.0 * 1000.0)
k7 line p8, p3, p9
outs (k7 * (a5 + (k4 * a6))), ((1.0 - k7) * (a5 + (k4 * a6)))
endin
