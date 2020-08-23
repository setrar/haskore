sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 1

instr 1
k1 line 0.0, p3, 200.0
a2 oscil 1.0, k1, 1
a3 oscil (((a2 + 1.0) * 0.5) * p5), p4, 1
out a3
endin
