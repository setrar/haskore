%.wav:	%.orc %.sco
	csound -W -b 1024 -d -m 0 -o $@ $^

play-%:	%.orc %.sco
	OPCODEDIR=/usr/lib/csound/plugins csound -n -o dac $^
