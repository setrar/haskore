sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 1

instr 1
k1 expon 1.0, 1.0, (1.0 / 100.0)
a2 oscil (k1 * ampdb(p5)), cpspch(p4), 3
a3 comb a2, 4.0, 0.22
out a3
endin
