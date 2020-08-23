sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 1

instr 1
k1 oscil ampdb(p5), (1.0 / p3), 10
a2 oscil k1, cpspch(p4), 11
out a2
endin
