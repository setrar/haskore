sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 2

instr 1
a1 pluck ampdb(p5), cpspch(p4), cpspch(p4), 0.0, 2, 1.5
k2 oscil 1.0, (1.0 / p3), 8
a3 pluck 6000.0, cpspch(p4), cpspch(p4), 0.0, 3, 0.3
outs ((0.65 * a3) + ((0.35 * k2) * a1)), ((0.35 * a3) + ((0.65 * k2) * a1))
endin
