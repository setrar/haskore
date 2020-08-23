sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 2

instr 1
k1 expon 1.0, 1.0, (1.0 / 100.0)
a2 oscil (k1 * ampdb(p5)), cpspch(p4), 3
a3 reverb a2, 0.3
outs ((0.65 * a2) + (0.35 * a3)), ((0.35 * a2) + (0.65 * a3))
endin
