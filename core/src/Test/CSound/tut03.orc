sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 1

instr 1
k1 line 1.0, p3, 0.0
a2 oscil (k1 * ampdb(p5)), cpspch(p4), 3
out a2
endin
