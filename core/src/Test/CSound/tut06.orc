sr     = 44100
kr     = 4410
ksmps  = 10.0
nchnls = 2

instr 1
k1 line 1.0, p3, 0.0
a2 oscil (k1 * ampdb(p5)), cpspch((p4 - 2.0)), 3
outs (0.8 * a2), (0.2 * a2)
endin

instr 2
k1 line 0.0, p3, 0.5
a2 oscil (k1 * ampdb(p5)), (cpspch(p4) * 5.0), 1
k3 linseg 0.0, (p3 * 0.83), 0.5, (p3 * (1.0 - 0.83)), 0.0
a4 oscil (k3 * ampdb(p5)), (cpspch(p4) * 3.0), 1
k5 linseg 0.0, (p3 * 0.67), 0.5, (p3 * (1.0 - 0.67)), 0.0
a6 oscil (k5 * ampdb(p5)), (cpspch(p4) * 2.5), 1
k7 linseg 0.0, (p3 * 0.5), 0.5, (p3 * (1.0 - 0.5)), 0.0
a8 oscil (k7 * ampdb(p5)), (cpspch(p4) * 2.0), 1
k9 linseg 0.0, (p3 * 0.33), 0.5, (p3 * (1.0 - 0.33)), 0.0
a10 oscil (k9 * ampdb(p5)), (cpspch(p4) * 1.1), 1
k11 linseg 0.0, (p3 * 0.17), 0.5, (p3 * (1.0 - 0.17)), 0.0
a12 oscil (k11 * ampdb(p5)), (cpspch(p4) * 1.0), 1
k13 line 0.5, p3, 0.0
a14 oscil (k13 * ampdb(p5)), (cpspch(p4) * 0.5), 1
outs (0.2 * ((((((a14 + a12) + a10) + a8) + a6) + a4) + a2)), (0.8 * ((((((a14 + a12) + a10) + a8) + a6) + a4) + a2))
endin
