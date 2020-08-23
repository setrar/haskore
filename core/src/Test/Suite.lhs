A module that automatically tests the function of several modules.

We use the (standard) package QuickCheck for automatic tests
on randomly generated data and
we use HUnit as a framework to run all tests.
Because of the lack of a package structure
we included the required modules from the HUnit project in Haskore.

The module must have the name \code{Main}
in order to be run by \code{runhugs}.

> module Main where

> import Equivalence((=?=), (==?==), )
> import qualified Equivalence
> import qualified Medium.Controlled      as CtrlMedium
> import qualified Medium.Controlled.List as CtrlMediumList
> import qualified Medium.Temporal as Temporal
> import qualified Medium

> import qualified Control.Monad.HT as M
> import Control.Monad.Trans.State (state, runState, )
> import Control.Monad.HT ((<=<), )
> import Control.Monad (liftM, liftM2, replicateM, when, )

> import qualified Data.List as List
> import qualified Data.List.HT as ListHT
> import           Data.Ratio (Ratio, (%), )
> import           Data.Maybe (isJust, )
> import           Data.Maybe.HT (toMaybe, )
> import System.Random(StdGen, RandomGen, mkStdGen, randomR, )

> import Haskore.Music        hiding (repeat, reverse, )
> import Haskore.Melody          as Melody
> import Haskore.Basic.Duration (wn, qn, en, (%+), )

> import qualified Haskore.Music as Music
> import qualified Haskore.Melody.Standard          as StdMelody
> import qualified Haskore.Music.GeneralMIDI        as MidiMusic
> import qualified Haskore.Music.Rhythmic           as RhyMusic
> import qualified Haskore.Basic.Pitch    as Pitch
> import qualified Haskore.Basic.Duration as Duration
> import qualified Haskore.Performance          as Performance
> import qualified Haskore.Performance.Fancy    as FancyPerformance
> import qualified Haskore.Performance.Default  as DefaultPerformance
> import qualified Haskore.Performance.Context  as Context
> import qualified Haskore.Performance.BackEnd  as PfBE
> import qualified Haskore.Process.Optimization as Optimization

> import qualified Haskore.Example.SelfSim        as SelfSim
> import qualified Haskore.Example.Flip           as Flip
> import qualified Haskore.Example.ChildSong6     as ChildSong6
> import qualified Haskore.Example.Ssf            as Ssf
> import qualified Haskore.Example.Fractal        as Fractal
> import qualified Haskore.Example.Kantate147     as Kantate147
> import qualified Haskore.Example.NewResolutions as NewResolutions
> import           Haskore.Example.Guitar         as Guitar
> import           Haskore.Example.Miscellaneous

> import qualified Haskore.Interface.MIDI.Render        as Render
> import qualified Haskore.Interface.MIDI.Write         as WriteMidi
> import qualified Haskore.Interface.MIDI.Read          as ReadMidi
> import qualified Haskore.Interface.MIDI.InstrumentMap as InstrMap
> import qualified Sound.MIDI.File         as MidiFile
> import qualified Sound.MIDI.File.Save    as SaveMidi
> import qualified Sound.MIDI.File.Load    as LoadMidi
> import qualified Sound.MIDI.Parser.Report as MidiReport
> import qualified Sound.MIDI.File.Event              as MidiEvent
> import qualified Sound.MIDI.File.Event.Meta         as MetaEvent
> import qualified Sound.MIDI.Message.Channel         as ChannelMsg
> import qualified Sound.MIDI.Message.Channel.Voice   as VoiceMsg

> import qualified Haskore.Interface.CSound.Orchestra as CSOrchestra
> import qualified Haskore.Interface.CSound.Score     as CSScore
> import qualified Haskore.Interface.CSound.Tutorial  as CSTutorial

> import qualified Medium.Controlled.ContextFreeGrammar as Grammar
> import qualified Haskore.Process.Format as MusicFormat

> import qualified Data.EventList.Relative.TimeBody as TimeList
> import qualified Numeric.NonNegative.Class   as NonNeg
> import qualified Numeric.NonNegative.Wrapper as NonNegW
> import Numeric.NonNegative.Class ((-|))

> import Test.QuickCheck
>    (Property, (==>), Arbitrary, resize, arbitrary, sized, choose,
>     Testable, Gen, oneof, frequency, )
> import qualified Test.QuickCheck       as QC
> import qualified Test.QuickCheck.Test  as QCT
> import qualified Test.HUnit      as HUnit
> import qualified Test.HUnit.Text as HUnitText

> import qualified Data.Accessor.Basic as Accessor

> import System.Cmd (system, )
> import qualified System.Exit as Exit
> import qualified Data.ByteString.Lazy as B

  import Debug.Trace (trace)


> midiDir, csoundDir :: FilePath
> midiDir   = "src/Test/MIDI/"
> csoundDir = "src/Test/CSound/"

> hugsPath :: String
> hugsPath = ":src:src/Haskore"

Some functions for connecting QuickCheck with HUnit.

> showResult :: QCT.Result -> String
> showResult (QCT.Success {}) = "success"
> showResult (QCT.GaveUp  {}) = "gave up"
> showResult (QCT.Failure {QCT.output = msg, QCT.numTests = n}) =
>    "failed at test " ++ show n ++ " with the arguments\n" ++ msg
> showResult (QCT.NoExpectedFailure {}) = "no expected failure"

> testUnit :: Testable a => String -> a -> HUnit.Test
> testUnit = testUnitOpt QCT.stdArgs

> testUnitOpt :: Testable a => QCT.Args -> String -> a -> HUnit.Test
> testUnitOpt opt label t =
>    HUnit.TestLabel label (HUnit.TestCase (
>       do result <- QCT.quickCheckWithResult opt t
>          HUnit.assertBool (showResult result) (QCT.isSuccess result)
>    ))



> sortLines :: String -> String
> sortLines = unlines . List.sort . lines

> diffFilesIA :: FilePath -> FilePath -> IO ()
> diffFilesIA file0 file1 =
>    system ("kompare "++file0++" "++file1) >> return ()
>    -- system ("tkdiff "++file0++" "++file1) >> return ()

> diffIA :: String -> String -> IO Bool
> diffIA orig new =
>    let file0 = "/tmp/orig.txt"
>        file1 = "/tmp/new.txt"
>        dif   = orig/=new
>    in  when dif
>           (do writeFile file0 orig
>               writeFile file1 new
>               diffFilesIA file0 file1) >>
>        return (not dif)

> assertEqualText :: String -> String -> String -> HUnit.Assertion
> assertEqualText preface expected actual =
>    let msg = (if null preface then "" else preface ++ "\n") ++
>               "expected: " ++ show expected ++ "\n but got: " ++ show actual
>    in  when (actual /= expected)
> --            (diffIA expected actual >>
>             (diffIA (sortLines expected) (sortLines actual) >>
>                         HUnit.assertFailure msg)



These tests checks if the MIDI files
generated for several examples is still the same
as these generated by the version of 2000.

> sortMidi :: MidiFile.T -> MidiFile.T
> sortMidi = MidiFile.progChangeBeforeSetTempo . MidiFile.sortEvents

> testMidiBin :: FilePath -> MidiFile.T -> HUnit.Test
> testMidiBin name stream =
>    HUnit.TestLabel name (HUnit.TestList
>       (testSaveMidi name stream : testReadMidi name : []))

> testSaveMidi :: FilePath -> MidiFile.T -> HUnit.Test
> testSaveMidi name stream = HUnit.TestCase $
>    do
> --   diffMidiBin name (sortMidi stream)
>      let path = midiDir++name++".mid"
>      let new  = SaveMidi.toByteString (sortMidi stream)
>      -- B.writeFile path new
>      orig <- B.readFile path
>      -- putStrLn (show (length orig) ++ " -- " ++ show (length stream))
>      HUnit.assertEqual "saveMidi" orig new

> equalMidi :: MidiFile.T -> MidiFile.T -> IO Bool
> equalMidi x y =
> --   diffIA (MidiFile.showLines x) (MidiFile.showLines y) >>
>    return (x == y)

> diffGenMidiBin :: (MidiFile.T -> String) -> FilePath -> MidiFile.T -> IO Bool
> diffGenMidiBin showFunc name new =
>    do
>      orig <- LoadMidi.fromFile (midiDir++name++".mid")
>      diffIA (showFunc orig) (showFunc new)

> diffMidiBin :: FilePath -> MidiFile.T -> IO Bool
> diffMidiBin = diffGenMidiBin MidiFile.showLines

Sorts the NoteOn and NoteOff MIDI events in the tracks.
Their order depends on rounding issues of performance time stamps.

> diffSortMidiBin :: FilePath -> MidiFile.T -> IO Bool
> diffSortMidiBin = diffGenMidiBin (MidiFile.showLines . MidiFile.sortEvents)

Sorts the lines of the formatted output and
thus tolerates changes in the order.
This post-processing is heavier than diffSortMidiBin.

> diffSortMidiBin' :: FilePath -> MidiFile.T -> IO Bool
> diffSortMidiBin' = diffGenMidiBin (sortLines . MidiFile.showLines)

> writeMusic ::
>    (InstrMap.ChannelTable MidiMusic.Instrument,
>     Context.T NonNegW.Float Float MidiMusic.Note, MidiMusic.T)
>        -> MidiFile.T
> writeMusic = WriteMidi.fromGMMusic

> testMidiStruct :: String -> MidiFile.T -> MidiFile.T -> HUnit.Assertion
> testMidiStruct name origFile newFile =
> --   diffSortMidiBin name newFile >>
>    HUnit.assertEqual
>       ("WriteMidi.fromMusic for "++name)
>       origFile
>       (MidiFile.sortEvents newFile)

Test the ReadMidi.toGMMusic function by reading and writing a test file.

> testReadMidi :: FilePath -> HUnit.Test
> testReadMidi name = HUnit.TestCase $
>    do
>      contents <- B.readFile (midiDir++name++".mid")
>      let midiFile =
>             either (error "on reading back MIDI") id $
>             MidiReport.result $
>             LoadMidi.maybeFromByteString contents
>      let midiFileRewritten = sortMidi $ writeMusic $ ReadMidi.toGMMusic midiFile
>      HUnit.assertEqual
>         "loadMidi"
>         contents
>         (SaveMidi.toByteString midiFile)
>      -- diffMidiBin name (MidiFile.sortEvents (writeMusic (ReadMidi.toGMMusic midiFile)))
>      {- Notes of zero duration bring note events out of order
>         if sorted with MidiFile.sortEvents.
>         What can we do against that? -}
>      HUnit.assertEqual
>         "ReadMidi.toGMMusic[0]"
>         midiFile
>         midiFileRewritten
>
>      HUnit.assertEqual
>         "ReadMidi.toGMMusic[1]"
> {-
>         (return (SaveMidi.toByteString (MidiFile.sortEvents midiFile)
>             == SaveMidi.toByteString midiFileRewritten))
> -}
>         contents
>         (SaveMidi.toByteString midiFileRewritten)
>      -- sorting necessary for test14b

> testReadMidiPure :: MidiFile.T -> HUnit.Assertion
> testReadMidiPure midiFile =
>    do
>      _ <- diffIA
>             (MidiFile.showLines (MidiFile.sortEvents midiFile))
>             (MidiFile.showLines (MidiFile.sortEvents
>                      (writeMusic (ReadMidi.toGMMusic midiFile))))
>      HUnit.assertEqual
>         ("ReadMidi.toGMMusic test")
>         (MidiFile.sortEvents midiFile)
>         (MidiFile.sortEvents (writeMusic (ReadMidi.toGMMusic midiFile)))

> setInstrMidi :: MidiFile.T
> setInstrMidi =
>    Render.generalMidi $
>       MidiMusic.fromMelodyNullAttr MidiMusic.Marimba   (c 0 qn ()) +:+
>       MidiMusic.fromMelodyNullAttr MidiMusic.Xylophone (e 0 qn ())

> midiPitch :: Int -> VoiceMsg.Pitch
> midiPitch = VoiceMsg.toPitch

> channel :: Int -> ChannelMsg.Channel
> channel = ChannelMsg.toChannel

> vel :: VoiceMsg.Velocity
> vel = VoiceMsg.normalVelocity

> voiceMsg :: VoiceMsg.T -> MidiEvent.T
> voiceMsg msg =
>    MidiEvent.MIDIEvent $ ChannelMsg.Cons (channel 0) $
>    ChannelMsg.Voice $ msg

> noteOn :: VoiceMsg.Pitch -> MidiEvent.T
> noteOn p =
>    voiceMsg $ VoiceMsg.NoteOn p vel

> noteOff :: VoiceMsg.Pitch -> MidiEvent.T
> noteOff p =
>    voiceMsg $ VoiceMsg.NoteOff p vel

> program :: VoiceMsg.Program -> MidiEvent.T
> program pgm =
>    voiceMsg $ VoiceMsg.ProgramChange pgm

> setTempo :: NonNegW.Int -> MidiEvent.T
> setTempo = MidiEvent.MetaEvent . MetaEvent.SetTempo

> setTempoMidi :: MidiFile.T
> setTempoMidi =
>    MidiFile.Cons MidiFile.Mixed (MidiFile.Ticks 12)
>       [TimeList.cons  0 (program (VoiceMsg.toProgram 0)) $
>        TimeList.cons  0 (setTempo 1000000) $
>        TimeList.cons  0 (noteOn  (midiPitch 60)) $
>        TimeList.cons 12 (noteOff (midiPitch 60)) $
>        TimeList.cons  0 (setTempo  500000) $
>        TimeList.cons  0 (noteOn  (midiPitch 60)) $
>        TimeList.cons 24 (noteOff (midiPitch 60)) $
>        TimeList.cons  0 (setTempo 2000000) $
>        TimeList.cons  0 (noteOn  (midiPitch 60)) $
>        TimeList.cons 12 (noteOff (midiPitch 60)) $
>        TimeList.empty]

> setTempoMusic ::
>    (InstrMap.ChannelTable MidiMusic.Instrument,
>     Context.T NonNegW.Float Float MidiMusic.Note, MidiMusic.T)
> setTempoMusic = ReadMidi.toGMMusic setTempoMidi

The velocities of the original tests were too strong.
MIDI spec says that a non-velocity-sensitive instrument
gets velocity value 64.

> hackVelocities :: MidiFile.T -> MidiFile.T
> hackVelocities = MidiFile.changeVelocity (127/64)

The tempo of the original files was made with 500000 microseconds
as unit.

> hackTempo :: MidiFile.T -> MidiFile.T
> hackTempo = MidiFile.resampleTime (1/2)


> testMIDI :: HUnit.Test
> testMIDI =
>    HUnit.TestLabel "comparison with MIDI files generated by former Haskore versions"
>       (HUnit.TestList (map (uncurry testMidiBin) (
>          ("test01",  hackVelocities  t1) :
>          ("test02",                  t2) :
>          ("test03",                  t3) :
>          ("test04",                  t4) :
>          ("test05",                  t5) :
>          ("test06",  hackVelocities  SelfSim.t6) :
>          ("test07",  hackVelocities  SelfSim.t7) :
>          ("test08",                  SelfSim.t8) :
>          ("test10",  hackVelocities SelfSim.t10) :
>          ("test13",  hackVelocities t13) :
>          ("test13a", hackVelocities t13a) :
>          ("test13b", hackVelocities t13b) :
>          ("test13c", hackVelocities t13c) :
>          ("test13d", hackVelocities t13d) :
>          ("test13e", hackVelocities t13e) :
>          ("test14",  hackVelocities t14) :
>          ("test14b",                t14b) :
>          ("test14c", hackVelocities t14c) :
>          ("test14d", hackVelocities t14d) :
>          ("Flip0", Render.generalMidiDeflt (Music.take 1 (withPiano Flip.song))) :
>          ("Flip1", Render.generalMidiDeflt (Music.take 5 (withPiano Flip.song1))) :
>          ("Flip2", Render.generalMidi (Music.take 7 Flip.song2)) :
>          ("Fractal", Render.generalMidiDeflt (Optimization.duration (withPiano Fractal.song))) :
>          ("Ssf", Render.generalMidiDeflt Ssf.song) :
>          ("ChildSong6", Render.generalMidiDeflt ChildSong6.song) :
>          ("NewResolutions", NewResolutions.midi) :
>          ("Kantate147", Kantate147.midi) :
> --         ("GuitarLegato",   Render.generalMidi Guitar.legatoSongMIDI) :
>          ("GuitarParallel", Render.generalMidi Guitar.parallelSongMIDI) :
>          [])))





Check generations of CSound files.

> testTutCSound ::
>    CSOrchestra.Output out =>
>       (String, CSScore.T, CSTutorial.TutOrchestra out) -> HUnit.Assertion
> testTutCSound = processTutCSound verifyResult


Three actions can be taken on a file to be compared with an old version.
All three share the same signature.

> verifyResult, diffResult, updateResult ::
>    String -> FilePath -> String -> HUnit.Assertion

The simple test if the new version is equal to the old one.
If not, emit an HUnit exception.

> verifyResult title fn str =
>    readFile fn >>=
>    flip (assertEqualText title) str
> --   HUnit.assertEqual title str

If the tests fail it can be useful to see the difference in detail
by calling 'kompare' or 'tkdiff'.

> diffResult _ fn str =
>    do str1 <- readFile fn
>       when (str1/=str)
>            (writeFile "/tmp/test" str >>
>             diffFilesIA fn "/tmp/test")

In case the changes are intended
you can just overwrite the old files with the new ones.

> updateResult _ fn str = writeFile fn str


> processTutCSound :: CSOrchestra.Output out =>
>    (String -> FilePath -> String -> HUnit.Assertion) ->
>        (String, CSScore.T, CSTutorial.TutOrchestra out) -> HUnit.Assertion
> processTutCSound proc (name, newScore, newOrchestra) =
>    do
>      proc
>        ("CSound orchestra: " ++ name)
>        (csoundDir++name++".orc")
>        (CSOrchestra.toString (CSTutorial.toOrchestra newOrchestra))
>
>      proc
>        ("CSound score: " ++ name)
>        (csoundDir++name++".sco")
>        (CSScore.toString newScore)

> processCSound :: CSOrchestra.Output out =>
>    (String -> FilePath -> String -> HUnit.Assertion) ->
>        (String, CSScore.T, CSOrchestra.T out) -> HUnit.Assertion
> processCSound proc (name, newScore, newOrchestra) =
>    do
>      proc
>        ("CSound orchestra: " ++ name)
>        (csoundDir++name++".orc")
>        (CSOrchestra.toString newOrchestra)
>
>      proc
>        ("CSound score: " ++ name)
>        (csoundDir++name++".sco")
>        (CSScore.toString newScore)


> diffCSound :: CSOrchestra.Output out =>
>    (String, CSScore.T, CSOrchestra.T out) -> IO ()
> diffCSound (name, newScore, newOrchestra) =
>    let orcName = csoundDir++name++".orc"
>        scoName = csoundDir++name++".sco"
>        tmpName = "/tmp/test"
>    in  do
>          CSOrchestra.save tmpName newOrchestra
>          diffFilesIA orcName (tmpName++".orc")
>          CSScore.save tmpName newScore
>          diffFilesIA scoName (tmpName++".sco")

> diffSortCSound :: CSOrchestra.Output out =>
>    (String, CSScore.T, CSOrchestra.T out) -> IO ()
> diffSortCSound (name, newScore, newOrchestra) =
>    let orcName = csoundDir++name++".orc"
>        scoName = csoundDir++name++".sco"
>    in  do
>          origOrchestra <- readFile orcName
>          _ <- diffIA (sortLines origOrchestra)
>                  (sortLines $ CSOrchestra.toString newOrchestra)
>          origScore <- readFile scoName
>          _ <- diffIA (sortLines origScore)
>                  (sortLines $ CSScore.toString newScore)
>          return ()

Compare with several files former versions have produced.

> testCSounds :: HUnit.Test
> testCSounds =
>    HUnit.TestLabel "comparison with csound files generated by former Haskore versions"
>       (HUnit.TestList (map HUnit.TestCase (
>          testTutCSound CSTutorial.tut1 :
>          testTutCSound CSTutorial.tut2 :
>          testTutCSound CSTutorial.tut3 :
>          testTutCSound CSTutorial.tut4 :
>          testTutCSound CSTutorial.tut5 :
>          testTutCSound CSTutorial.tut6 :
>          testTutCSound CSTutorial.tut7 :
>          testTutCSound CSTutorial.tut8 :
>          testTutCSound CSTutorial.tut9 :
>          testTutCSound CSTutorial.tut10 :
>          testTutCSound CSTutorial.tut11 :
>          testTutCSound CSTutorial.tut12 :
>          testTutCSound CSTutorial.tut13 :
>          testTutCSound CSTutorial.tut14 :
>          testTutCSound CSTutorial.tut15 :
>          testTutCSound CSTutorial.tut16 :
>          testTutCSound CSTutorial.tut17 :
>          testTutCSound CSTutorial.tut18 :
>          testTutCSound CSTutorial.tut19 :
>          testTutCSound CSTutorial.tut20 :
>          testTutCSound CSTutorial.tut21 :
>          testTutCSound CSTutorial.tut22 :
>          testTutCSound CSTutorial.piano :
>          testTutCSound CSTutorial.reedy :
>          testTutCSound CSTutorial.reedy2 :
>          testTutCSound CSTutorial.flute :
>       [])))



These tests check for certain bugs that have already removed
and will hopefully never return!

It should be possible get a prefix of some representation of infinite music.
We define a function which asks for some character
of the string representation.
If the implementations are ill, we'll get lost in an infinite loop.


> withPiano :: Melody.T () -> MidiMusic.T
> withPiano = MidiMusic.fromMelodyNullAttr MidiMusic.AcousticGrandPiano

> performanceFromMIDIMusic ::
>    MidiMusic.T -> Performance.T NonNegW.Rational Rational MidiMusic.Note
> performanceFromMIDIMusic =
>    FancyPerformance.fromMusic

> testShowInf :: Show a => Int -> a -> Bool
> testShowInf n x = show x !! n /= '\000'

> testInfinitePerformance :: [HUnit.Test]
> testInfinitePerformance =
>    let -- an infinite rest loop won't eventually result in an empty list
>        -- p = Render.performance (line (repeat wnr))
>        m    = withPiano (line (repeat (a 0 wn ())))
>        p    = performanceFromMIDIMusic m
>        midi = Render.generalMidiDeflt m
>    in  [HUnit.TestCase
>           (HUnit.assertBool "performance" (testShowInf 80 p)),
>         HUnit.TestCase
>           (HUnit.assertBool "MIDI file" (testShowInf 200 midi))]

If the definition of (+:+) is improper
the check will fail on infinite application.

> testInfiniteConcat :: HUnit.Test
> testInfiniteConcat =
>    let m = foldr1 (+:+) (repeat (a 0 wn ()))
>    in  HUnit.TestCase
>          (HUnit.assertBool "application of (+:+)" (testShowInf 100 m))

Check if the partition of infinite streams works properly.

This one fails
  mel = a 0 wn () +:+ b 0 wn ()  =:=  rest qn +:+ mel

whereas this one works
  mel = a 0 wn () +:+ b 0 wn ()  =:=  rest qn +:+ repeat (c 0 wn ())

*Main> let mel = a 0 wn () +:+ b 0 wn ()  =:=  rest wn +:+ undefined

*Main> mel
Parallel [Serial [Primitive (Atom (1%1) (Just (Note {noteAttrs = (), notePitch = (0,A)}))),Primitive (Atom (1%1) (Just (Note {noteAttrs = (), notePitch = (0,B)})))],Serial [Primitive (Atom (1%1) Nothing)*** Exception: Prelude.undefined
*Main> performanceFromMIDIMusic (withPiano mel)
*** Exception: Prelude.undefined

*Main> Control.Monad.Reader.runReader (Performance.monadFromMusic Haskore.Performance.Player.defltMap mel) Context.deflt

> testInfinitePartition :: HUnit.Test
> testInfinitePartition =
>    let -- mel = a 0 wn () +:+ b 1 wn ()  =:=  line [rest qn, mel]
>        mel = a 0 wn () +:+ b 1 wn ()  =:=  rest qn +:+ mel
>        p   = ((1,Pitch.A)<=) . Accessor.get Melody.notePitch
>        (melA, melB) = Music.partition p mel
>        pfA = performanceFromMIDIMusic (withPiano melA)
>        pfB = performanceFromMIDIMusic (withPiano melB)
>    in  HUnit.TestCase
>          (HUnit.assertBool "partition"
>             (testShowInf 200 pfA  &&  testShowInf 200 pfB))

> testInfinitePerformancePartition :: HUnit.Test
> testInfinitePerformancePartition =
>    let m   = withPiano (Music.repeat (a 0 wn () +:+ b 0 wn ()))
>        pf  = performanceFromMIDIMusic m
>        p   = ((0,Pitch.A)<=) . MidiMusic.pitch .
>                 MidiMusic.body . Performance.eventNote
>        pfs = TimeList.partition p pf
>    in  HUnit.TestCase
>          (HUnit.assertBool "partition" (testShowInf 200 pfs))

> testInfinity :: HUnit.Test
> testInfinity = HUnit.TestLabel "infinite music" (HUnit.TestList
>    (testInfiniteConcat :
>     testInfinitePartition :
>     testInfinitePerformancePartition :
>     testInfinitePerformance))

\function{randomTree}
generates a somehow random tree of notes.
We use an ascending sequence of pitches,
because MIDI can't distinguish between parallel notes of the same pitch.

\begin{haskelllisting}

> randomTree :: Pitch.Absolute -> StdGen -> Melody.T ()
> randomTree p g0 =
>    let (d',     g1) = randomR (0, 6) g0
>        (opn,    g2) = randomR (0, length ops - 1) g1
>        (tmpNum, g3) = randomR (1, 4) g2
>        (tmpDen, g4) = randomR (1, 4) g3
>        ops = [(+:+), flip (+:+), (=:=),
>               \m0 m1 -> changeTempo (tmpNum%+tmpDen) (m0+:+m1)]
>    in  (ops !! opn)
>        (note (Pitch.fromInt p) (d'%+4) ())
>        (randomTree (succ p) g4)

> instance Arbitrary note => Arbitrary (Music.Primitive note) where
>    arbitrary = arbitraryPrimitive

> arbitraryPrimitive :: Arbitrary note => Gen (Music.Primitive note)
> arbitraryPrimitive =
>    liftM2 Music.Atom
>       (liftM2 (%+) (choose (1,8)) (choose (1,8)))
>       (frequency
>           [(3, liftM Just arbitrary),
>            (1, return Nothing)])

> instance Arbitrary Music.Control where
>    arbitrary =
>       oneof
>          [liftM Music.Tempo
>                    (M.until (0<) (resize 20 arbitrary)),
>           liftM Music.Transpose (resize 20 arbitrary)]

> instance Arbitrary attr => Arbitrary (Melody.Note attr) where
>    arbitrary =
>       liftM2 (\attr n -> (Melody.Note attr
>                  (Pitch.fromInt (mod n 100))))
>          arbitrary (resize 100 arbitrary)

> {-
> chooseEnum :: (Enum a, Bounded a) => Gen a
> chooseEnum =
>    let fromEnumGen :: Enum a => Gen a -> a -> Int
>        fromEnumGen _ = fromEnum
>        gen = liftM toEnum
>              (choose (fromEnumGen gen minBound, fromEnumGen gen maxBound))
>    in  gen
> -}

> instance (Arbitrary instr, Arbitrary drum) =>
>             Arbitrary (RhyMusic.NoteBody drum instr) where
>    arbitrary =
>       liftM2 RhyMusic.Tone
>          arbitrary
>          (liftM (\n -> Pitch.fromInt (mod n 100)) (resize 100 arbitrary))

> instance (Arbitrary instr, Arbitrary drum) =>
>             Arbitrary (RhyMusic.Note drum instr) where
>    arbitrary =
>       liftM2 RhyMusic.Note
>          (liftM abs arbitrary)
>          arbitrary

> instance (NonNeg.C time, Arbitrary time, Arbitrary note) =>
>              Arbitrary (PfBE.Event time note) where
>    arbitrary = liftM2 PfBE.Event arbitrary arbitrary


> {-
> -- we need this e.g. for Equivalence.propTempoRest0
> instance (Integral a, Arbitrary a) => Arbitrary (Ratio a) where
> --   arbitrary = liftM2 (%+) arbitrary (M.until (0/=) arbitrary)
> {-      M.until (0/=) leads to infinite loop in some cases,
>         probably because of 'size' reduced to zero. -}
>    arbitrary =
>       liftM2 (\numer denom -> numer % (if denom==0 then 1 else denom))
>              arbitrary arbitrary
> -}

> {-
> instance Arbitrary Char where
>    arbitrary =
>       frequency
>          [(26, choose ('a','z')),
>           (26, choose ('A','Z')),
>           (10, choose ('0','9'))]
> -}

> instance (Temporal.C a, Arbitrary a, Arbitrary control) =>
>       Arbitrary (CtrlMediumList.T control a) where
>    arbitrary =
>       let sizedTree 0 = liftM Medium.prim arbitrary
>           sizedTree n =
>              let subTree m = replicateM m (resize (div n m) arbitrary)
>              in  frequency
>                     [(3, liftM Medium.prim     arbitrary),
>                      (1, liftM Medium.serial   (choose (0,n) >>= subTree)),
>                      (1, liftM Medium.parallel (choose (0,n) >>= subTree)),
>                      (1, liftM2 CtrlMedium.control arbitrary arbitrary)]
>       in  sized sizedTree
> {-
>    arbitrary =
>       let sizedTree 0 = liftM Medium.List.Prim arbitrary
>           sizedTree n =
>              let halfTree = sizedTree (div n 2)
>              in  frequency
>                     [(3, liftM  Medium.List.Prim arbitrary),
>                      (1, liftM2 (Medium.+:+) halfTree halfTree),
>                      (1, liftM2 (Medium.=:=) halfTree halfTree)]
>       in  sized sizedTree
> -}

\end{haskelllisting}

> propBackEndPerformance ::
>    PfBE.T NonNegW.Rational MidiMusic.Note -> Bool
> propBackEndPerformance p =
>    let performanceFromMusic :: MidiMusic.T -> PfBE.T NonNegW.Rational MidiMusic.Note
>        performanceFromMusic =
>           PfBE.fromPerformance (const (const id)) .
>           (flip asTypeOf (undefined ::
>               Performance.T NonNegW.Rational Rational MidiMusic.Note)) .
>           DefaultPerformance.fromMusicModifyContext (Context.setDur 1)
>    in  TimeList.normalize p ==
>           TimeList.normalize (performanceFromMusic (PfBE.toMusic p))

> testPerformance :: HUnit.Test
> testPerformance =
>    HUnit.TestLabel "performance"
>       (testUnit "backend" propBackEndPerformance)

Check certain properties of \function{Music.take}.

> propTakeDurFinite, propDropDurFinite,
>    propTakeDurInfinite, propDropDurInfinite,
>    propTakeDurInfinite', propDropDurInfinite',
>    propTakeTooLong, propDropTooLong :: Dur -> MidiMusic.T -> Property

> propTakeDurFinite d' m  =
>    d' >= 0  ==>
>       dur (Music.take d' m) == min d' (dur m)
> propDropDurFinite d' m  =
>    d' >= 0  ==>
>       dur (Music.drop d' m) == dur m -| d'

The following two properties are only true if the music has infinite duration.
We construct an infinite music
by cycling all serial compositions of the music.
In order to get something for cycling
we have to preserve the existence of a serial composition.
Empty compositions are also bad for \function{cycle}
but instead of checking for them we optimize them away.
I hope that the optimization won't destroy some interesting pathologic examples.

> propTakeDurInfinite d' m  =
>    let mOpt = Optimization.composition m
>    in  d' >= 0  &&  atLeastOneSerial mOpt  ==>
>           dur (Music.take d' (cycleMusic mOpt)) == d'
> propDropDurInfinite d' m  =
>    let mOpt = Optimization.composition m
>    in  d' >= 0  &&  atLeastOneSerial mOpt  ==>
>           dur (Music.take 1 (Music.drop d' (cycleMusic mOpt))) == 1

The preconditions are fulfilled too seldomly.

> propTakeDurInfinite' d' m  =
>    d' >= 0  &&  nonEmptySerials m  &&  atLeastOneSerial m  ==>
>       dur (Music.take d' (cycleMusic m)) == d'
> propDropDurInfinite' d' m  =
>    d' >= 0  &&  nonEmptySerials m  &&  atLeastOneSerial m  ==>
>       dur (Music.take 1 (Music.drop d' (cycleMusic m))) == 1

> propTakeTooLong d' m  =
>    d' >= 0  ==>
>       Music.take (dur m + d') m =?= m
> propDropTooLong d' m  =
>    d' >= 0  ==>
>       Music.drop (dur m + d') m =?= rest 0

Duplicate of Utility.maximum0.
We don't use that one because Utility is a private module.

> maximum0 :: (Ord a, Num a) => [a] -> a
> maximum0 = List.foldl' max 0

Check if the serial compositions in a music are non-empty,
otherwise \function{cycle} fails.

> nonEmptySerials :: MidiMusic.T -> Bool
> nonEmptySerials = isJust .
>    Music.foldList
>       (const . Just) (flip const)
>       ((\d' -> toMaybe (d'/=0) d') . sum <=< sequence)
>       (liftM maximum0 . sequence)

This fails for the music (line [chord []])
    Music.foldList (const (const True)) (flip const) or and

Check if a music contains at least one serial composition,
otherwise the music won't become infinite using \function{cycleMusic}.

> atLeastOneSerial :: MidiMusic.T -> Bool
> atLeastOneSerial =
>    Music.foldList (const (const False)) (flip const) (const True) or

Make music infinite by cycling serial compositions.

> cycleMusic :: MidiMusic.T -> MidiMusic.T
> cycleMusic = Music.mapList (,) (flip const) cycle id

> testTakeDrop :: HUnit.Test
> testTakeDrop =
>    -- testUnitBig = testUnitOpt QCB.defOpt{QCB.no_of_tests=10000}
>    HUnit.TestLabel "take, drop" (HUnit.TestList (
>       testUnit "take/dur/finite"   propTakeDurFinite   :
>       testUnit "drop/dur/finite"   propDropDurFinite   :
>       testUnit "take/dur/infinite" propTakeDurInfinite :
>       testUnit "drop/dur/infinite" propDropDurInfinite :
>       testUnit "take/too long"     propTakeTooLong     :
>       testUnit "drop/too long"     propDropTooLong     :
>    []))

Check certain properties of \function{Music.reverse}.

> propReverse :: MidiMusic.T -> Bool
> propReverse  =  Music.reverse . Music.reverse ==?== id

> testReverse :: HUnit.Test
> testReverse =
>    HUnit.TestLabel "reverse" (testUnit "inverse" propReverse)

Check properties of \function{Music.filter} et al.

> pitchTest :: Pitch.Absolute -> RhyMusic.Note drum instr -> Bool
> pitchTest pitch =
>    (pitch<=) . Pitch.toInt . MidiMusic.pitch . MidiMusic.body

> propFilterPartition, propParallelPartition, propPartitionMaybe ::
>    Pitch.Absolute -> MidiMusic.T -> Bool

> propFilterPartition pitch m  =
>    let p = pitchTest pitch
>    in  Music.partition p m ==
>        (Music.filter p m,  Music.filter (not . p) m)

> propParallelPartition pitch  =
>    let p = pitchTest pitch
>    in  id  ==?==  uncurry (=:=) . Music.partition p

> propPartitionMaybe pitch m  =
>    let p = pitchTest pitch
>    in  Music.partition p m  ==
>        Music.partitionMaybe (\n -> toMaybe (p n) n) m

> testFilter :: HUnit.Test
> testFilter =
>    HUnit.TestLabel "filter" (HUnit.TestList (
>       testUnit "filter partition"   propFilterPartition   :
>       testUnit "parallel partition" propParallelPartition :
>       testUnit "partition maybe"    propPartitionMaybe    :
>    []))

Check if \module{Optimization} simplifies some examples according
to the laws given in \secref{equivalence}.

> propOptAll, propOptRest, propOptComposition, propOptDuration,
>    propOptTempo, propOptTranspose, propOptVolume
>       :: MidiMusic.T -> Bool
> propOptAll          =  id ==?== Optimization.all
> propOptRest         =  id ==?== Optimization.rest
> propOptComposition  =  id ==?== Optimization.composition
> propOptDuration     =  id ==?== Optimization.duration
> propOptTempo        =  id ==?== Optimization.tempo
> propOptTranspose    =  id ==?== Optimization.transpose
> propOptVolume       =  id ==?== Optimization.volume

\end{haskelllisting}


Randomly permutate a list.
For this purpose we generate a random \type{Bool} value
for each item of the list
which specifies in what sublist it is inserted.
Both sublists are then concatenated hereafter.
By repeating this procedure several times
the list should be somehow randomly ordered.

Some notes about perfect shuffling from Oleg:
\url{http://okmij.org/ftp/Haskell/misc.html#perfect-shuffle}

\begin{haskelllisting}

> shuffle :: RandomGen g => [a] -> g -> ([a],g)
> shuffle x g0 =
>    let (choices,g1) =
>           runState (mapM (const (state (randomR (False,True)))) x) g0
>        xc = zip x choices
>    in  (map fst (uncurry (++) (ListHT.partition snd xc)), g1)

> testOptimization :: HUnit.Test
> testOptimization =
>    let controls0 =
>           [Music.changeTempo 3,
>            Music.changeTempo 1,
>            Music.changeTempo (1/3),
>            Music.transpose 1,
>            Music.transpose 2,
>            Music.transpose 3]
>        controls1 =
>           [Music.changeTempo 2,
>            Music.changeTempo 3,
>            Music.changeTempo 5,
> --           Music.phrase (Music.Accent 1.01),
>            Music.transpose (-3),
>            Music.transpose ( 0),
>            Music.transpose ( 3)]
>        mixer ctrls g' = List.take 10 (map fst
>              (iterate (uncurry shuffle) (ctrls,g')))
>        rcs0 = mixer controls0 (mkStdGen 142)
>        rcs1 = mixer controls1 (mkStdGen 857)
>        mOrig cs0 cs1 =
>           foldr id
>              (c 1 en () =:= rest qn =:= foldr id (a 0 qn () +:+ rest 0) cs1)
>              cs0
>        mOptOrigs = map Optimization.all (zipWith mOrig rcs0 rcs1)
>        mOpt =
>           Music.transpose 6
>              (chord [c 1 en (), qnr, Music.changeTempo 30 (a 0 qn ())])
>    in {-
>       mapM (putStrLn . MusicFormat.prettyMelody) mOptOrigs >>
>       putStrLn (MusicFormat.prettyMelody mOpt) >>
>       -}
>       HUnit.TestLabel "optimization" (HUnit.TestList (
>          HUnit.TestCase (HUnit.assertBool "shuffled controls"
>                             (all (mOpt ==) mOptOrigs)) :
>          testUnit "all"         propOptAll         :
>          testUnit "rest"        propOptRest        :
>          testUnit "composition" propOptComposition :
>          testUnit "duration"    propOptDuration    :
>          testUnit "tempo"       propOptTempo       :
>          testUnit "transpose"   propOptTranspose   :
>          testUnit "volume"      propOptVolume      :
>       []))


Check if the precedence of serial composition
is higher than that of parallel composition.

> testPrecedence :: HUnit.Test
> testPrecedence =
>    HUnit.TestLabel "precedence" (HUnit.TestList [
>       HUnit.TestCase
>          (HUnit.assertBool "+:+/=:="
>                 ( c 0 wn () +:+ e 0 wn ()  =:= g 0 wn () ==
>                  (c 0 wn () +:+ e 0 wn ()) =:= g 0 wn ())),
>       HUnit.TestCase
>          (HUnit.assertBool "=:=/+:+"
>                 (c 0 wn () =:=  e 0 wn () +:+ g 0 wn () ==
>                  c 0 wn () =:= (e 0 wn () +:+ g 0 wn ())))])


Test for structure analysis.
To check the integrity of the structure analysis
we turn a song into grammar and expand it again.
The original song and the expanded one should be literally equivalent.

\begin{haskelllisting}

> grammarExample0, grammarExample1 :: Melody.T ()
> grammarExample0 = Music.take 17 Flip.core
> grammarExample1 = line (List.take 20 (cycle [c 0 qn (), e 0 wn (), g 0 wn ()]))

> propGrammar :: MidiMusic.T -> Bool
> propGrammar =
>    id ==?== Grammar.toMedium .
>             Grammar.fromMedium (map (("part"++).(:[])) ['A'..]) 2

> testGrammar :: HUnit.Test
> testGrammar =
>    let test name m0 =
>           HUnit.TestCase
>              (HUnit.assertBool name (propGrammar (withPiano m0)))
>    in  {- diffIA (MidiFile.showLines (Render.generalMidiDeflt m0))
>                  (MidiFile.showLines (Render.generalMidiDeflt m1)) >>
>           diffIA (MidiFile.showLines (MidiFile.sortEvents (Render.generalMidiDeflt Kantate147.song)))
>                  (MidiFile.showLines (MidiFile.sortEvents (Render.generalMidiDeflt (Grammar.toMedium Kantate147.grammar)))) >> -}
>        HUnit.TestLabel "structure analysis" (HUnit.TestList [
>           test "example0" grammarExample0,
>           test "example1" grammarExample1,
>           -- testUnit "inverse" propGrammar,
>           HUnit.TestCase
>              (HUnit.assertBool "kantate147"
>                  (withPiano (changeTempo (4%+3) Kantate147.song) =?=
>                   withPiano (Grammar.toMedium Kantate147.grammar)))])

\end{haskelllisting}

Check if a music is properly formatted,
that is check if the output is syntactically correct
and if the generated module generates the same MIDI file
as we obtain directly.

\begin{haskelllisting}

> ctrlMusic :: Melody.T ()
> ctrlMusic =
>    let n0 = c 1 (1/23) ()
>        n1 = c 1 qn ()
>        r0 = rest (1/23)
>        r1 = rest qn
>    in  changeTempo (2/3) (n0 +:+ r0) =:= transpose 3 (n1 +:+ r1) =:=
>        chord [changeTempo (2/3) n0, transpose (-3) n1,
>               changeTempo 7 r0, transpose 4 r1]

> testFormatMusic :: HUnit.Test
> testFormatMusic = HUnit.TestCase $
>    do writeFile "GeneratedTest.hs" (unlines
>          ["module GeneratedTest where",
>           "import Haskore.Basic.Duration((%+))",
>           "import Haskore.Music",
>           "import Haskore.Melody.Standard",
>           "import Haskore.Music.GeneralMIDI as MidiMusic",
>           "import Haskore.Interface.MIDI.Render as Render",
>           "main = Render.fileFromGeneralMIDIMusic \"test.mid\" song",
>           "song = MidiMusic.fromStdMelody MidiMusic.AcousticGrandPiano $ " ++
>                   MusicFormat.prettyMelody
>                      (StdMelody.fromMelodyNullAttr ctrlMusic)])
>       exitCode <-
>          if False
>            then system ("echo 'main\n:q' | hugs -98 -P"++hugsPath++" GeneratedTest")
>            else system ("ghc -e main -i"++hugsPath++" GeneratedTest")
>       HUnit.assertEqual
>          "running Haskell interpreter"
>          exitCode
>          Exit.ExitSuccess
>       midi <- B.readFile "test.mid"
>       let expectedMidi =
>              SaveMidi.toByteString (Render.generalMidi (withPiano ctrlMusic))
>       -- B.writeFile "expected.mid" expectedMidi
>       HUnit.assertEqual
>          "formatting music"
>          expectedMidi
>          midi

> testFormat :: HUnit.Test
> testFormat =
>    HUnit.TestLabel "composition" $ HUnit.TestList $
>       testFormatMusic :
>       HUnit.TestCase
>           (HUnit.assertBool "formatting duration" Duration.propToString) :
>       []

\end{haskelllisting}

\begin{haskelllisting}

> testComposition :: HUnit.Test
> testComposition =
>    HUnit.TestLabel "composition" (HUnit.TestList (
>       HUnit.TestLabel "tempo" (HUnit.TestList (
>          testUnit "neutral"       Equivalence.propTempoNeutral :
>          testUnit "fuse"          Equivalence.propTempoTempo :
>          testUnit "commutativity" Equivalence.propTempoCommutativity :
>          testUnit "transpose/commutativity"
>                                   Equivalence.propTempoTransposeCommutativity :
>          testUnit "serial"        Equivalence.propTempoSerial :
>          testUnit "parallel"      Equivalence.propTempoParallel :
>          testUnit "rest0"         Equivalence.propTempoRest0 :
>       [])) :
>       HUnit.TestLabel "transpose" (HUnit.TestList (
>          testUnit "neutral"       Equivalence.propTransposeNeutral :
>          testUnit "fuse"          Equivalence.propTransposeTranspose :
>          testUnit "commutativity" Equivalence.propTransposeCommutativity :
>          testUnit "serial"        Equivalence.propTransposeSerial :
>          testUnit "parallel"      Equivalence.propTransposeParallel :
>          testUnit "rest0"         Equivalence.propTransposeRest0 :
>       [])) :
>       HUnit.TestLabel "serial" (HUnit.TestList (
>          testUnit "associativity" Equivalence.propSerialAssociativity :
>          testUnit "neutral0"      Equivalence.propSerialNeutral0 :
>          testUnit "neutral1"      Equivalence.propSerialNeutral1 :
>          testUnit "parallel0"     Equivalence.propSerialParallel0 :
>          testUnit "parallel1"     Equivalence.propSerialParallel1 :
>       [])) :
>       HUnit.TestLabel "parallel" (HUnit.TestList (
>          testUnit "associativity" Equivalence.propParallelAssociativity :
>          testUnit "commutativity" Equivalence.propParallelCommutativity :
>       [])) :
>    []))

\end{haskelllisting}

\begin{haskelllisting}

> allTests :: HUnit.Test
> allTests =
>    HUnit.TestList $
>      testComposition :
>      testTakeDrop :
>      testReverse :
>      testFilter :
>      testOptimization :
>      testInfinity :
>      testPrecedence :
>      testPerformance :
>      testGrammar :
>      testFormat :
>      testCSounds :
>      testMIDI :
>      []

\end{haskelllisting}

\begin{haskelllisting}

> main :: IO ()
> main =
>    do
>       when False $
>          mapM_ putStrLn $
>          zipWith (\num path -> show num ++ " - " ++ HUnitText.showPath path)
>                  [(1::Int)..] $
>          HUnit.testCasePaths allTests
> --      putStrLn "tests disabled"
>       counts <- HUnitText.runTestTT allTests
>       when (HUnit.errors counts + HUnit.failures counts > 0)
>            (error "Test suite encountered errors.")

\end{haskelllisting}
