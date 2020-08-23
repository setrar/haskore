sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 1

instr 1
a1 randi ((ampdb(p5) / 100.0) * p6), p7
a2 oscili (a1 + ampdb(p5)), (1.0 / p3), 14
a3 oscili a2, cpspch(p4), 15
out a3
endin
