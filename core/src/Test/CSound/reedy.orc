sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 1

instr 1
k1 oscil ampdb(p5), (1.0 / p3), 12
a2 oscil k1, cpspch(p4), 13
out a2
endin
