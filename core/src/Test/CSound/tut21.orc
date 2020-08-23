sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 2

instr 1
a8 init 0
a5 init 0
a1 delay a8, 0.271
a2 atone a1, 1000.0
k3 expon 1.0, 1.0, (1.0 / 100.0)
a4 oscil (k3 * ampdb(p5)), cpspch(p4), 3
a5 = (a4 + a2)
a6 delay a8, 0.311
a7 tone a6, 500.0
a8 = (a4 + a7)
outs a8, a5
endin
