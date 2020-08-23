> module Haskore.Interface.CSound.OrchestraFunction where

> {- a fast variant of 'elem'
>    precondition: list must be sorted
>    This could be replaced by Data.Map when it is widely available. -}
> elemSorted :: (Ord a) => a -> [a] -> Bool
> elemSorted x ys =
>    case dropWhile (GT==) (map (compare x) ys) of
>       EQ:_ -> True
>       _ -> False

> allowedArgs :: [(String, [Int])] -> String -> Int -> Bool
> allowedArgs table name count =
>    maybe True (elemSorted count) (lookup name table)

> -- This should be a Data.Map in future.
> argCountTable :: [(String, [Int])]
> argCountTable = [("=", [1]),
>                  ("-", [1]),
>                  ("a", [1]),
>                  ("abs", [1]),
>                  ("active", [1]),
>                  ("adsr", [4, 5]),
>                  ("adsyn", [4]),
>                  ("adsynt", [6, 7]),
>                  ("aftouch", [0..2]),
>                  ("alpass", [3..5]),
>                  ("ampdb", [1]),
>                  ("ampdbfs", [1]),
>                  ("ampmidi", [1, 2]),
>                  ("areson", [3..5]),
>                  ("aresonk", [3..5]),
>                  ("atone", [2, 3]),
>                  ("atonek", [2, 3]),
>                  ("atonex", [2..4]),
>                  ("babo", [7..9]), -- gives two outputs
>                  ("balance", [2..4]),
>                  ("bamboo", [2..8]),
>                  ("bbcutm", [6..9]),
>                  ("bbcuts", [7..10]), -- gives two outputs
>                  ("betarand", [3]),
>                  ("bexprnd", [1]),
>                  ("biquad", [7, 8]),
>                  ("biquada", [7, 8]),
>                  ("birnd", [1]),
>                  ("bqrez", [3..5]),
>                  ("butterbp", [3, 4]),
>                  ("butterbr", [3, 4]),
>                  ("butterhp", [2, 3]),
>                  ("butterlp", [2, 3]),
>                  ("button", [1]), -- gui
>                  ("buzz", [4, 5]),
>                  ("cabasa", [2..5]),
>                  ("cauchy", [1]),
>                  ("cent", [1]),
>                  ("chanctrl", [2..4]),
>                  ("changed", [1..]),
>                  ("checkbox", [1]), -- gui
>                  ("clear", [1..]), -- no output
>                  ("clfilt", [4..8]),
>                  ("clip", [3, 4]),
>                  ("clockoff", [1]), -- no output
>                  ("clockon", [1]), -- no output
>                  ("comb", [3..5]),
>                  ("control", [1]), -- gui
>                  ("convolve", [2, 3]), -- gives 1-4 outputs
>                  ("cos", [1]),
>                  ("cosh", [1]),
>                  ("cosinv", [1]),
>                  ("cps2pch", [2]),
>                  ("cpsmidi", [0]),
>                  ("cpsmidib", [0, 1]),
>                  ("cpsoct", [1]),
>                  ("cpspch", [1]),
>                  ("cpstmid", [1]),
>                  ("cpstun", [3]),
>                  ("cpstuni", [2]),
>                  ("cpsxpch", [4]),
>                  ("cpuprc", [2]), -- no output
>                  ("cross2", [6]),
>                  ("crunch", [2..5]),
>                  ("ctrl14", [5, 6]),
>                  ("ctrl21", [6, 7]),
>                  ("ctrl7", [4, 5]),
>                  ("ctrlinit", [3, 5..65]), -- no output
>                  ("cuserrnd", [3]),
>                  ("dam", [6]),
>                  ("db", [1]),
>                  ("dbamp", [1]),
>                  ("dbfsamp", [1]),
>                  ("dcblock", [1, 2]),
>                  ("dconv", [3]),
>                  ("delay", [1, 2]),
>                  ("delay1", [1, 2]),
>                  ("delayr", [1]),
>                  ("delayw", [1]), -- no output
>                  ("deltap", [1]),
>                  ("deltap3", [1]),
>                  ("deltapi", [1]),
>                  ("deltapn", [1]),
>                  ("deltapx", [2]),
>                  ("deltapxw", [3]), -- no output
>                  ("diff", [1, 2]),
>                  ("diskin", [2..6]), -- gives 1-4 outputs
>                  ("dispfft", [3..6]), -- no output
>                  ("display", [2..4]), -- no output
>                  ("distort", [5]),
>                  ("divz", [3]),
>                  ("downsamp", [1, 2]),
>                  ("dripwater", [2..8]),
>                  ("dumpk", [4]), -- no output
>                                  -- several other dump functions with no output
>                  ("duserrnd", [1]),
>                  ("envlpx", [7, 8]),
>                  ("envlpxr", [6..8]),
>                  -- "event" cannot be created because strings are not OrcExps
>                  ("exp", [1]),
>                  ("expon", [3]),
>                  ("exprand", [1]),
>                  ("expseg", [3, 5..]),
>                  ("expsega", [3, 5..]),
>                  ("expsegr", [5, 7..]),
>                  ("filelen", [1]),
>                  ("filenchnls", [1]),
>                  ("filepeak", [1, 2]),
>                  ("filesr", [1]),
>                  ("filter2", [3..]),
>                  ("fin", [4..]), -- no output
>                  ("fini", [4..]), -- no output
>                  ("fink", [4..]), -- no output
>                  ("fiopen", [2]), -- takes a string argument
>                  ("flanger", [3, 4]),
>                  ("flashtxt", [2]), -- no output, gui
>                  -- several different gui elements occur here
>                  ("fmb3", [11]),
>                  ("fmbell", [11]),
>                  ("fmmetal", [11]),
>                  ("fmpercfl", [11]),
>                  ("fmrhode", [11]),
>                  ("fmvoice", [11]),
>                  ("fmwurlie", [11]),
>                  ("fof", [12..15]),
>                  ("fof2", [14, 15]),
>                  ("fofilter", [4, 5]),
>                  ("fog", [13..16]),
>                  ("fold", [2]),
>                  ("follow", [2]),
>                  ("follow2", [3]),
>                  ("foscil", [6, 7]),
>                  ("focsili", [6, 7]),
>                  ("fout", [3..]), -- no output
>                  ("fouti", [4..]), -- no output
>                  ("foutir", [4..]), -- no output
>                  ("foutk", [3..]), -- no output
>                  ("fprintks", [2..]), -- no output
>                  ("fprints", [2..]), -- no output
>                  ("frac", [1]),
>                  ("ftchnls", [1]),
>                  ("ftgen", [5..]),
>                  ("ftlen", [1]),
>                  ("ftload", [3..]),
>                  ("ftloadk", [4..]),
>                  ("ftlptim", [1]),
>                  ("ftmorf", [3]), -- no output
>                  ("ftsave", [3..]), -- no output
>                  ("ftsavek", [4..]), -- no output
>                  ("ftsr", [1]),
>                  ("gain", [2..4]),
>                  ("gauss", [1]),
>                  ("gbuzz", [6, 7]),
>                  ("gogobel", [8]),
>                  ("grain", [9, 10]),
>                  ("grain2", [6..9]),
>                  ("grain3", [11..13]),
>                  ("granule", [17..23]),
>                  ("guiro", [2..7]),
>                  ("harmon", [8]),
>                  ("hilbert", [1]), -- gives two outputs
>                  ("hrtfer", [4]), -- gives two outputs, takes a string
>                  ("hsboscil", [6..8]),
>                  ("i", [1]),
>                  ("ihold", [0]), -- no output
>                  ("in", [0]),
>                  ("in32", [0]), -- gives 32 outputs
>                  ("inch", [1]),
>                  ("inh", [0]), -- gives six outputs
>                  ("init", [1]),
>                  ("initc14", [4]), -- no output
>                  ("initc21", [5]), -- no output
>                  ("initc7", [3]), -- no output
>                  ("ino", [0]), -- gives eight outputs
>                  ("inq", [0]), -- gives four outputs
>                  ("ins", [0]), -- gives two outputs
>                  ("int", [1]),
>                  ("integ", [1, 2]),
>                  ("invalue", [1]), -- takes a string
>                  ("inx", [0]), -- gives 16 outputs
>                  ("inz", [1]), -- no output
>                  ("jitter", [3]),
>                  ("jitter2", [7]),
>                  ("jspline", [3]),
>                  ("ktableseg", [3, 5..]), -- no output
>                  ("lfo", [2, 3]),
>                  ("limit", [3]),
>                  ("line", [3]),
>                  ("linen", [4]),
>                  ("linenr", [4]),
>                  ("lineto", [2]),
>                  ("linrand", [1]),
>                  ("linseg", [3, 5..]),
>                  ("linsegr", [5, 7..]),
>                  ("locsend", [0]), -- gives 2 or 4 outputs
>                  ("locsig", [4]), -- gives 2 or 4 outputs
>                  ("log", [1]),
>                  ("log10", [1]),
>                  ("logbtwo", [1]),
>                  ("loopseg", [4, 6..]),
>                  ("lorenz", [8, 9]), -- gives three outputs
>                  ("loscil", [3..10]), -- gives 1-2 outputs
>                  ("loscil3", [3..10]), -- gives 1-2 outputs
>                  ("lowpass2", [3, 4]),
>                  ("lowres", [3, 4]),
>                  ("lowresx", [3..5]),
>                  ("lpf18", [3]),
>                  ("lpfreson", [2]),
>                  ("lphasor", [1..6]),
>                  ("lpinterp", [3]),
>                  ("lposcil", [5, 6]),
>                  ("lposcil3", [5, 6]),
>                  ("lpread", [2..4]), -- gives four outputs
>                  ("lpreson", [1]),
>                  ("lpshold", [4, 6..]),
>                  ("lpslot", [1]), -- no output
>                  ("mac", [2, 4..]),
>                  ("maca", [1..]),
>                  ("madsr", [4..6]),
>                  ("mandel", [4]), -- gives two outputs
>                  ("mandol", [7, 8]),
>                  ("marimba", [9..11]),
>                  ("massign", [2]),
>                  ("maxalloc", [2]), -- no output
>                  ("max_k", [3]),
>                  ("mclock", [1]), -- no output
>                  ("mdelay", [5]), -- no output
>                  ("metro", [1, 2]),
>                  ("midic14", [4, 5]),
>                  ("midic21", [5, 6]),
>                  ("midic7", [3, 4]),
>                  ("midichannelaftertouch", [1..3]), -- no output
>                  ("midichn", [0]),
>                  ("midicontrolchange", [2..4]), -- no output
>                  ("midictrl", [1..3]),
>                  ("mididefault", [2]), -- no output
>                  ("midiin", [0]), -- gives four outputs
>                  ("midinoteoff", [2]), -- no output
>                  ("midinoteoncps", [2]), -- no output
>                  ("midinoteonkey", [2]), -- no output
>                  ("midinoteonoct", [2]), -- no output
>                  ("midinoteonpch", [2]), -- no output
>                  ("midion", [3]), -- no output
>                  ("midion2", [4]), -- no output
>                  ("midiout", [4]), -- no output
>                  ("midipitchbend", [1..3]), -- no output
>                  ("midipolyaftertouch", [2..4]), -- no output
>                  ("midiprogramchange", [1]), -- no output
>                  ("mirror", [3]),
>                  ("moog", [9]),
>                  ("moogladder", [3, 4]),
>                  ("moogvcf", [3..5]),
>                  ("moscil", [5]), -- no output
>                  ("mpulse", [2, 3]),
>                  ("mrtmsg", [1]), -- no output
>                  ("multitap", [1, 3..]),
>                  ("mute", [1, 2]), -- no output
>                  ("mxadsr", [4..6]),
>                  ("nestedap", [5, 7, 9, 10]),
>                  ("nlfilt", [6]),
>                  ("noise", [2]),
>                  ("noteoff", [3]), -- no output
>                  ("noteon", [3]), -- no output
>                  ("noteondur", [4]), -- no output
>                  ("noteondur2", [4]), -- no output
>                  ("notnum", [0]),
>                  ("nreverb", [3..8]),
>                  ("nrpn", [3]), -- no output
>                  ("nsamp", [1]),
>                  ("nstrnum", [1]), -- takes string
>                  ("ntrpol", [3..5]),
>                  ("octave", [1]),
>                  ("octcps", [1]),
>                  ("octmidi", [0]),
>                  ("octmidib", [0, 1]),
>                  ("octpch", [1]),
>                  ("oscbnk", [19..26]),
>                  ("oscil", [3, 4]),
>                  ("oscil1", [4]),
>                  ("oscil1i", [4]),
>                  ("oscil3", [3, 4]),
>                  ("oscili", [3, 4]),
>                  ("oscilikt", [3..5]),
>                  ("osciliktp", [3, 4]),
>                  ("oscilikts", [5, 6]),
>                  ("osciln", [4]),
>                  ("oscils", [3, 4]),
>                  ("oscilx", [4]),
>                  ("out", [1]), -- no output
>                  ("out32", [32]), -- no output
>                  ("outc", [1..]), -- no output
>                  ("outch", [2, 4..]), -- no output
>                  ("outh", [6]), -- no output
>                  ("outiat", [4]), -- no output
>                  ("outic", [5]), -- no output
>                  ("outic14", [6]), -- no output
>                  ("outipat", [5]), -- no output
>                  ("outipb", [4]), -- no output
>                  ("outipc", [4]), -- no output
>                  ("outkat", [4]), -- no output
>                  ("outkc", [5]), -- no output
>                  ("outkc14", [6]), -- no output
>                  ("outkpat", [5]), -- no output
>                  ("outkpb", [4]), -- no output
>                  ("outkpc", [4]), -- no output
>                  ("outo", [8]), -- no output
>                  ("outq", [4]), -- no output
>                  ("outq1", [1]), -- no output
>                  ("outq2", [1]), -- no output
>                  ("outq3", [1]), -- no output
>                  ("outq4", [1]), -- no output
>                  ("outs", [2]), -- no output
>                  ("outs1", [1]), -- no output
>                  ("outs2", [1]), -- no output
>                  ("outvalue", [2]), -- no output, takes a string
>                  ("outx", [16]), -- no output
>                  ("outz", [1]), -- no output
>                  ("p", [1]),
>                  ("pan", [4..6]), -- gives four outputs
>                  ("pareq", [4..6]),
>                  ("pcauchy", [1]),
>                  ("pchbend", [0..2]),
>                  ("pchmidi", [0]),
>                  ("pchmidib", [0, 1]),
>                  ("pchoct", [1]),
>                  ("peak", [1]),
>                  ("pgmassign", [2]), -- no output, takes a string
>                  ("phaser1", [4, 5]),
>                  ("phaser2", [7]),
>                  ("phasor", [1, 2]),
>                  ("phasorbnk", [3, 4]),
>                  ("pinkish", [1..5]),
>                  ("pitch", [5..13]), -- gives two outputs
>                  ("pitchamdf", [3..8]), -- gives two outputs
>                  ("planet", [10..12]), -- gives three outputs
>                  ("pluck", [5..7]),
>                  ("poisson", [1]),
>                  ("polyaft", [1..3]),
>                  ("port", [2, 3]),
>                  ("portk", [2, 3]),
>                  ("poscil", [3, 4]),
>                  ("poscil3", [3, 4]),
>                  ("pow", [2, 3]),
>                  ("powoftwo", [1]),
>                  ("prealloc", [2]), -- no output, takes a string
>                  ("print", [1..]), -- no output
>                  ("printk", [2, 3]), -- no output
>                  ("printk2", [1, 2]), -- no output
>                  ("printks", [2..]), -- no output
>                  ("prints", [1..]), -- no output
>                  ("product", [2..]),
>                  ("pset", [1..]), -- no output
>                  ("pvadd", [5..10]), -- takes a string
>                  ("pvbufread", [2]), -- no output
>                  ("pvcross", [5, 6]), -- takes a string
>                  ("pvinterp", [9]), -- takes a string
>                  ("pvoc", [3..7]), -- takes a string
>                  ("pvread", [3]), -- takes a string, gives two outputs
>                  -- lots of pvoc functions
>                  ("rand", [1..4]),
>                  ("randh", [2..5]),
>                  ("randi", [2..5]),
>                  ("random", [2]),
>                  ("randomh", [3]),
>                  ("randomi", [3]),
>                  ("readclock", [1]),
>                  ("readk", [3, 4]), -- takes a string
>                  -- several readk functions
>                  ("reinit", [1]), -- no output
>                  ("release", [0]),
>                  ("repluck", [6]),
>                  ("reson", [3..5]),
>                  ("resonk", [3..5]),
>                  ("resonr", [3..5]),
>                  ("resonx", [3..6]),
>                  ("resonxk", [3, 6]),
>                  ("resony", [5..8]),
>                  ("resonz", [3..5]),
>                  ("reverb", [2, 3]),
>                  ("rezzy", [3, 5]),
>                  ("rms", [1..3]),
>                  ("rnd", [1]),
>                  ("rnd31", [2, 3]),
>                  ("rspline", [4]),
>                  ("rtclock", [0]),
>                  ("s16b14", [1, 7..]), -- gives 16 outputs
>                  ("s32b14", [1, 7..]), -- gives 32 outputs
>                  ("samphold", [2..4]),
>                  ("sandpaper", [2..5]),
>                  ("scanhammer", [4]), -- no output
>                  ("scans", [4, 5]),
>                  ("scantable", [7]),
>                  ("scanu", [18]), -- no output
>                  ("schedkwhen", [6..]), -- no output
>                  ("schedkwhennamed", [6..]), -- no output, takes a string
>                  ("schedule", [3..]), -- no output, takes a string
>                  ("schedwhen", [4..]), -- no output, takes a string
>                  ("seed", [1]), -- no output
>                  ("sekere", [2..5]),
>                  ("semitone", [1]),
>                  ("sense", [0]),
>                  ("sensekey", [0]),
>                  ("seqtime", [5]),
>                  ("setctrl", [3]), -- gui, no output
>                  ("setksmps", [1]), -- no output
>                  ("sfilist", [1]), -- no output
>                  ("sfinstr", [6..8]), -- gives two outputs
>                  ("sfinstr3", [6..8]), -- gives two outputs
>                  ("sfinstr3m", [6..8]),
>                  ("sfinstrm", [6..8]),
>                  ("sfload", [1]), -- takes a string
>                  ("sfpassign", [2]), -- no output
>                  ("sfplay", [5..7]), -- gives two outputs
>                  ("sfplay3", [5..7]), -- gives two outputs
>                  ("sfplay3m", [5..7]),
>                  ("sfplaym", [5..7]),
>                  ("sfplist", [1]), -- no output
>                  ("sfpreset", [4]),
>                  ("shaker", [5, 6]),
>                  ("sin", [1]),
>                  ("sinh", [1]),
>                  ("sininv", [1]),
>                  ("sleighbells", [2..8]),
>                  ("slider16", [1, 6..]), -- gives 16 outputs
>                  -- lots of sliders with multiple (eg, 32) outputs
>                  ("sndwarp", [10]), -- gives 1-2 outputs
>                  ("sndwarpst", [10]), -- gives 2-4 outputs
>                  ("soundin", [1..4]), -- gives multiple outputs, takes a string
>                  ("soundout", [2, 3]), -- takes a string, no output
>                  ("space", [6]), -- gives four outputs
>                  ("spat3d", [9, 10]), -- gives four outputs
>                  ("spat3di", [7, 8]), -- gives four outputs
>                  ("spat3dt", [8, 9]), -- no output
>                  ("spdist", [4]),
>                  ("specaddm", [2, 3]),
>                  ("specdiff", [1]),
>                  ("specdisp", [2, 3]), -- no output
>                  ("specfilt", [2]),
>                  ("spechist", [1]),
>                  ("specptrk", [8..13]), -- gives two outputs
>                  ("specscap", [3]),
>                  ("specsum", [1, 2]),
>                  ("spectrum", [4..9]),
>                  ("splitrig", [5..]), -- no output
>                  ("spsend", [0]), -- gives four outputs
>                  ("sqrt", [1]),
>                  ("statevar", [3, 5]), -- gives four outputs
>                  ("stix", [2..5]),
>                  ("streson", [3]),
>                  ("strset", [2]), -- no output, takes a string
>                  ("subinstr", [1..]), -- gives 1-8 outputs, takes a string
>                  ("subinstrinit", [1..]), -- no output, takes a string
>                  ("sum", [1..]),
>                  ("svfilter", [3, 4]), -- gives three outputs
>                  ("syncgrain", [8]),
>                  ("table", [2..5]),
>                  ("table3", [2..5]),
>                  ("tablecopy", [2]), -- no output
>                  ("tablegpw", [1]), -- no output
>                  ("tablei", [2..5]),
>                  ("tableicopy", [2]), -- no output
>                  ("tableigpw", [1]), -- no output
>                  ("tableikt", [2..5]),
>                  ("tableimix", [9]), -- no output
>                  ("tableiw", [3..6]), -- no output
>                  ("tablekt", [2..5]),
>                  ("tablemix", [9]), -- no output
>                  ("tableng", [1]),
>                  ("tablera", [3]),
>                  ("tableseg", [3, 5..]), -- no output
>                  ("tablew", [3..6]), -- no output
>                  ("tablewa", [3]),
>                  ("tablewkt", [3..6]), -- no output
>                  ("tablexkt", [4..7]),
>                  ("tablexseg", [3, 5..]), -- no output
>                  ("tambourine", [2..8]),
>                  ("tan", [1]),
>                  ("tanh", [1]),
>                  ("taninv", [1]),
>                  ("taninv2", [2]),
>                  ("tbvcf", [5, 6]),
>                  ("tempest", [10..12]),
>                  ("tempo", [2]), -- no output
>                  ("tempoval", [0]),
>                  ("timeinstk", [0]),
>                  ("timeinsts", [0]),
>                  ("timek", [0]),
>                  ("times", [0]),
>                  ("tival", [0]),
>                  ("tlineto", [3]),
>                  ("tone", [2, 3]),
>                  ("tonek", [2, 3]),
>                  ("tonex", [2..4]),
>                  ("transeg", [4, 7..]),
>                  ("trigger", [3]),
>                  ("trigseq", [6..]), -- no output
>                  ("trirand", [1]),
>                  ("turnoff", [0]), -- no output
>                  ("turnon", [1, 2]), -- no output
>                  ("unirand", [1]),
>                  ("upsamp", [1]),
>                  ("urd", [1]),
>                  ("vadd", [3]),
>                  ("valpass", [4..6]),
>                  ("vbap16", [2..4]), -- gives 16 outputs
>                  -- various vbap functions that give outputs =/= 1
>                  ("vco", [4..10]),
>                  ("vco2", [2..6]),
>                  ("vco2ft", [2, 3]),
>                  ("vco2ift", [2, 3]),
>                  ("vco2init", [1..6]),
>                  ("vcomb", [4..6]),
>                  ("vdelay", [3, 4]),
>                  ("vdelay3", [3, 4]),
>                  ("vdelayx", [4, 5]),
>                  ("vdelayxq", [7, 8]), -- gives four outputs
>                  ("vdelayxs", [5, 6]), -- gives two outputs
>                  ("vdelayxw", [4, 5]),
>                  ("vdelayxwq", [7, 8]), -- gives four outputs
>                  ("vdelayxqs", [5, 6]), -- gives two outputs
>                  ("veloc", [0..2]),
>                  ("vexp", [3]), -- no output
>                  ("vexpseg", [5, 7..]), -- no output
>                  ("vibes", [9]),
>                  ("vibr", [3]),
>                  ("vibrato", [9, 10]),
>                  ("vincr", [2]), -- no output
>                  ("vlowres", [5]),
>                  ("vlinseg", [5, 7..]), -- no output
>                  ("vmult", [3]), -- no output
>                  ("voice", [8]),
>                  ("vpow", [3]), -- no output
>                  -- several functions for reading and writing vectors
>                  ("vpvoc", [3..5]), -- takes a string
>                  ("waveset", [2, 3]),
>                  ("weibull", [2]),
>                  ("wgbow", [7, 8]),
>                  ("wgbowedbar", [5..9]),
>                  ("wgbrass", [7, 8]),
>                  ("wgclar", [9, 10]),
>                  ("wgflute", [9..12]),
>                  ("wgpluck", [7]),
>                  ("wgpluck2", [5]),
>                  ("wguide1", [4]),
>                  ("wguide2", [7]),
>                  ("wrap", [3]),
>                  ("wterrain", [8]),
>                  ("xadsr", [4, 5]),
>                  ("xin", [0]), -- gives multiple outputs
>                  ("xout", [1..]), -- no output
>                  ("xscanmap", [3, 4]), -- gives two outputs
>                  ("xscansmap", [5, 6]), -- no output
>                  ("xscans", [4, 5]),
>                  ("xscanu", [18]), -- no output
>                  ("xtratim", [1]), -- no output
>                  ("xyin", [5..7]), -- gives two outputs
>                  ("zacl", [2]), -- no output
>                  ("zakinit", [2]), -- no output
>                  ("zamod", [2]),
>                  ("zar", [1]),
>                  ("zarg", [2]),
>                  ("zaw", [2]), -- no output
>                  ("zawm", [2, 3]), -- no output
>                  ("zfilter2", [5..]),
>                  ("zir", [1]),
>                  ("ziw", [2]), -- no output
>                  ("ziwm", [2, 3]), -- no output
>                  ("zkcl", [2]), -- no output
>                  ("zkmod", [2]),
>                  ("zkr", [1]),
>                  ("zkw", [2]), -- no output
>                  ("zkwm", [2, 3]) -- no output
>                                     ]
