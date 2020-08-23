\subsubsection{Intervals}
\seclabel{intervals}

% \url{http://en.wikipedia.org/wiki/Interval_(music)}

In music theory, an interval is the difference
(a ratio or logarithmic measure) in pitch between two notes
and often refers to those two notes themselves (otherwise known as a dyad).

Here we list some common names for some possible intervals.

\begin{haskelllisting}

> module Haskore.Basic.Interval where

> unison, minorSecond, majorSecond, minorThird, majorThird,
>  fourth, fifth, minorSixth, majorSixth, minorSeventh, majorSeventh,
>  octave, octaveMinorSecond, octaveMajorSecond, octaveMinorThird,
>  octaveMajorThird, octaveFourth, octaveFifth, octaveMinorSixth,
>  octaveMajorSixth, octaveMinorSeventh, octaveMajorSeventh :: Integral a => a
> unison       =  0
> minorSecond  =  1
> majorSecond  =  2
> minorThird   =  3
> majorThird   =  4
> fourth       =  5
> fifth        =  7
> minorSixth   =  8
> majorSixth   =  9
> minorSeventh = 10
> majorSeventh = 11
> octave       = 12
> octaveMinorSecond  = octave + minorSecond 
> octaveMajorSecond  = octave + majorSecond 
> octaveMinorThird   = octave + minorThird  
> octaveMajorThird   = octave + majorThird  
> octaveFourth       = octave + fourth      
> octaveFifth        = octave + fifth       
> octaveMinorSixth   = octave + minorSixth  
> octaveMajorSixth   = octave + majorSixth  
> octaveMinorSeventh = octave + minorSeventh
> octaveMajorSeventh = octave + majorSeventh

\end{haskelllisting}
