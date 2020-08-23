sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 2

instr 1
k1 line 0.0, p3, p7
a2 oscil ((cpspch(p4) * p6) * k1), (cpspch(p4) * p6), 1
a3 oscil (((cpspch(p4) * p6) * k1) * 3.0), (((cpspch(p4) * p6) * 2.0) + a2), 1
a4 oscil ampdb(p5), (cpspch(p4) + a3), 1
k5 line p8, p3, p9
k6 oscil 1.0, (1.0 / p3), 4
outs ((k6 * k5) * a4), ((k6 * (1.0 - k5)) * a4)
endin
