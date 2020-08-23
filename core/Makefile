
BASICS   = Basic/Pitch.lhs Basic/Duration.lhs Basic/Tempo.lhs Basic/Interval.lhs Basic/Scale.lhs \
	   Composition/Trill.lhs Composition/Chord.lhs Composition/ChordType.lhs \
	   Composition/Drum.lhs Composition/Rhythm.lhs \
	   Melody.lhs Melody/Standard.lhs \
	   Music.lhs Music/Standard.lhs Music/Rhythmic.lhs Music/GeneralMIDI.lhs \
	   Performance/Player.lhs Performance/BackEnd.lhs \
	   Performance/Default.lhs Performance/Fancy.lhs \
	   Performance.lhs Performance/Context.hs \
	   General/Utility.lhs General/Map.hs \
	   General/LoopTreeTagged.lhs General/LoopTreeRecursive.lhs \
	   General/LoopTreeTaggedGen.lhs General/LoopTreeRecursiveGen.lhs \
	   Process/Optimization.lhs Process/Format.lhs \
	   Interface/MML.lhs Interface/MED/Text.hs

MEDIA    = Medium.hs Medium/Temporal.hs \
	   Medium/Plain/List.hs Medium/Plain/Binary.hs Medium/Plain/ContextFreeGrammar.lhs \
	   Medium/Controlled/List.hs Medium/LabeledControlled/List.hs Medium/Controlled.hs \
	   Medium/Controlled/ContextFreeGrammar.lhs


MIDI     = Interface/MIDI.lhs \
           $(patsubst %, Interface/MIDI/%, \
	       InstrumentMap.lhs Note.lhs \
	       Read.lhs Write.lhs Render.lhs )

CSOUND   = Interface/CSound.lhs \
           $(patsubst %, Interface/CSound/%, \
	       Score.lhs InstrumentMap.lhs SoundMap.hs Note.lhs \
               Generator.lhs Orchestra.lhs OrchestraFunction.lhs Tutorial.lhs)

AUTOTRACK = $(patsubst %, Interface/AutoTrack/%.lhs, \
              ChartBar ChordChart ChordSymbol EventChart \
              Instrument ScaleChart Style Transposeable)


EXAMPLES = $(patsubst %, Example/%, \
	       Miscellaneous.lhs \
	       Ssf.lhs NewResolutions.lhs \
	       ChildSong6.lhs Kantate147.hs WhiteChristmas.hs \
	       SelfSim.lhs Fractal.hs Flip.hs Guitar.lhs)

MODULES = $(BASICS) $(EXAMPLES) $(MIDI) $(CSOUND) $(AUTOTRACK)

MODULEPATH = src

# http://www.gnu.org/software/automake/manual/make/Syntax-of-Functions.html#Syntax-of-Functions
colon:= :
empty:=
space:= $(empty) $(empty)

# exclude installed versions of Haskore, because we want to use the local one
HUGS_PACKAGE_PATH = \
   {Hugs}/libraries:{Hugs}/packages/*:$(subst $(space),$(colon),$(patsubst %,/usr/local/lib/hugs/packages/%,event-list midi markov-chain non-negative special-functors data-accessor transformers monoid-transformer explicit-exception binary utility-ht))
#   $(subst $(space),$(colon),$(patsubst %,{Hugs}/packages/%,event-list midi markov-chain non-negative record-access))


GHC_MODULES = $(patsubst %, src/%, $(MEDIA) \
                  Test/Equivalence.lhs Test/Suite.lhs) \
              $(patsubst %, src/Haskore/%, \
                  $(MODULES) )

GHC_DEPENDS = $(GHC_MODULES)

# names of literate modules after removing literary information
UNLIT_MODULES = $(patsubst %.lhs, %.hs, $(patsubst %.hs, , $(GHC_MODULES)))


HUGS_MODULES = $(patsubst %, src/%, $(MEDIA)) \
               $(patsubst %, src/Haskore/%, $(MODULES))


TEX_FILES = $(patsubst %, src/Doc/%, \
              Tutorial.tex Discussion.tex Introduction.tex Macros.tex Related.tex \
              Pics/haskore.tex Pics/midi.tex)

PICS = equiv poly

PDF_PICS = $(patsubst %, src/Doc/Pics/%.pdf, $(PICS))



.INTERMEDIATE:	$(UNLIT_MODULES) PlayTmp.hs

.PHONY:	all clean distclean cabal-all compile ghci hugs playmidi \
	pdf autotrack-ps doc \
	test test-compile test-hugs testcases debug \
	fluid

all:	compile

clean:
	-(cd build && rm -f `find . -name "*.hi"` `find . -name "*.o"`)
	-rm -f $(UNLIT_MODULES)
	-rm -f src/Doc/Pics/*.pdf
	-rm -f Tutorial.*

distclean:	clean
	-rm -f dist/doc/Tutorial.pdf

test:   test-compile pdf autotrack-ps testcases

compile:	hugs cabal-all

test-compile:	test-hugs cabal-all

cabal-all:
	runhaskell Setup configure --user --enable-tests -fbuildExamples
	runhaskell Setup build
	runhaskell Setup haddock

testcases:	cabal-all
	./dist/build/test-haskore/test-haskore +RTS -M32m -c30 -RTS

# start ghci using compiled objects from Cabal's 'dist/build' directory
ghci:	cabal-build
	ghci +RTS -M256m -c30 -RTS -Wall \
	   -odirdist/build -hidirdist/build -i:$(MODULEPATH):src/Test

hugs:	$(HUGS_MODULES)
# this version wouldn't stop on a failure :-(
#	echo ":quit" | hugs $(HUGS_MODULES)
# this worked as long as most of the modules were Haskell 98 compliant
#	hugs -P:$(MODULEPATH) $(HUGS_MODULES)

# for hugs version 2002-11
#	hugs +N -98 -h1000000 -P:$(MODULEPATH) $(HUGS_MODULES)

# for hugs version 2005-03
	hugs -98 -h1000000 -P$(MODULEPATH):$(HUGS_PACKAGE_PATH) $(HUGS_MODULES)

test-hugs:	$(HUGS_MODULES) # hugs
	@echo "***** Enter :q in order to continue testing. *****"
	hugs -98 -h1000000 -P$(MODULEPATH):$(HUGS_PACKAGE_PATH) $(HUGS_MODULES)

%.hs:	%.lhs
	unlit $< $@


pdf:	$(TEX_FILES) $(PDF_PICS) $(GHC_MODULES)
# src/Doc needed for Tutorial.bbl
	TEXINPUTS=src:src/Doc:$(TEXINPUTS) pdflatex $<
	mkindex Tutorial
	TEXINPUTS=src:src/Doc:$(TEXINPUTS) pdflatex $<
# the directory should coincide with the one created by Cabal
	mkdir -p dist/doc
	cp Tutorial.pdf dist/doc/Tutorial.pdf

%.pdf:	%.eps
	epstopdf $<

autotrack-ps:
	cd src/Haskore/Interface/AutoTrack/ && make doc


playmidi:
# install in NEdit menu:
#  cd haskore_dir/src/ ; make playmidi MODULE=%

# doesn't work, because Hugs supports only one visible module at the prompt
#	 echo TestMidi.testTimidity `xargs echo` | hugs +N -98 -h1000000 -P:$(MODULEPATH) Interface/MIDI/TestMidi.lhs $(MODULE)

	echo module Main where > PlayTmp.hs
	echo import TestMidi >> PlayTmp.hs
	MODULE_LHS=`basename $(MODULE) .lhs` && echo import `basename $$MODULE_LHS .hs` >> PlayTmp.hs
	echo main = TestMidi.testTimidity '('`xargs echo`')' >> PlayTmp.hs
	runhugs +N -98 -h1000000 -P:$(MODULEPATH) PlayTmp.hs

# start fluidsynth as server
# search fluidsynth port with pmidi -l
# play MIDI files using pmidi -p 128:0 src/Test/MIDI/ChildSong6.mid
fluid:
	fluidsynth --verbose /usr/share/sounds/sf2/Vintage_Dreams_Waves_v2.sf2

jack:
	jackd -d alsa -r 44100 -n 3 -d hw:0 &

# better start jack separately and then run 'make fluidjack'
# because otherwise fluidsynth starts jack itself but with inappropriate settings

# There is a difference to fluidsynth of Suse 9.2:
# If you simply start fluidsynth, it will report incoming MIDI messages,
# but you do not hear anything.
# It seems that you must connect fluidsynth to JACK manually.
# You can list the available JACK ports with
#    jack_lsp
# Cf. http://ubuntuforums.org/archive/index.php/t-480233.html
fluidjack:
	(usleep 3000000; make fluidconnect) &
	fluidsynth -a jack --verbose /usr/share/sounds/sf2/Vintage_Dreams_Waves_v2.sf2

fluidconnect:
	jack_connect fluidsynth:left  alsa_pcm:playback_1
	jack_connect fluidsynth:tight alsa_pcm:playback_2

timidity:
	timidity -iA -B1,8

debug:
	echo $(GHC_DEPENDS)
