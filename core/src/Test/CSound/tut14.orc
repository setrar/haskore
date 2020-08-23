sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 1

instr 1
k1 line 0.0, p3, 70.0
a2 buzz ampdb(p5), cpspch(p4), k1, 1
out a2
endin
