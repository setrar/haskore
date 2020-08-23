sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 1

instr 1
a7 init 0
a1 delay a7, 0.271
a2 atone a1, 1000.0
a3 delay a7, 0.311
a4 tone a3, 500.0
k5 expon 1.0, 1.0, (1.0 / 100.0)
a6 oscil (k5 * ampdb(p5)), cpspch(p4), 3
a7 = (a6 + (0.7 * (a4 + a2)))
out a7
endin
