HASKOREOLD = /home/thielema/programming/haskell/haskore_old
HASKORE    = /home/thielema/programming/haskell/haskore

HC      = ghc
HC_OPTS = -cpp -i:$(HASKOREOLD)/src:$(HASKORE)/src/GHC

SRCS = Main.lhs Option.lhs Transposeable.lhs Style.lhs ChordSymbol.lhs Instrument.lhs \
          ChartBar.lhs ChordChart.lhs EventChart.lhs ScaleChart.lhs

.SUFFIXES : .o .hi .lhs .hc .s .hs

all:	track doc

track:  $(SRCS)
	$(HC) $(HC_OPTS) --make -o $@ $<

doc:	AutoTrack.dvi AutoTrack.ps

AutoTrack.dvi: $(SRCS)
	latex $<
	mv Main.dvi $@

%.ps:	%.dvi
	dvips -o $@ $<
