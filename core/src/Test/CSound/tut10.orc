sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 2

instr 1
k1 line 0.0, p3, p7
a2 oscil ((cpspch(p4) * p6) * k1), (cpspch(p4) * p6), 1
a3 oscil ampdb(p5), (cpspch(p4) + a2), 1
k4 line p8, p3, p9
k5 oscil 1.0, (1.0 / p3), 4
outs ((k5 * k4) * a3), ((k5 * (1.0 - k4)) * a3)
endin
