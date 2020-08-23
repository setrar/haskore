% from AutoTrack by Stefan Ratschan

\documentclass[10pt]{article}

\usepackage[a4paper, margin=3cm]{geometry}
\usepackage{url}

\usepackage{color}
\definecolor{darkgrey}{rgb}{0.4,0.4,0.4}
\definecolor{lightgrey}{rgb}{0.95,0.95,0.95}


\usepackage{listings}

\lstset{%
   language=Haskell,
   showstringspaces=false,
   basicstyle=\ttfamily,
   keywordstyle=\textbf,
   commentstyle=\highlightcomment,
   backgroundcolor=\color{lightgrey}}

\newcommand\highlightcomment[1]{\textsl{\color{darkgrey}#1}}
\lstnewenvironment{haskelllisting}
   {\lstset{language=Haskell,gobble=2,firstline=2}}{}
\lstnewenvironment{haskellblock}
   {\mbox{}\\\lstset{language=Haskell}}{}


\newcommand{\STitle}{\texttt{AutoTrack}}

\title{\STitle}
\author{Stefan Ratschan}

\begin{document}

\maketitle

\section{Introduction}

This software has a short term and a long term goal. The short term goal is a tool for
creating practicing tracks for musicians. For this it is already usable: You feed it with
some chord chart, tell it the style of music, and it outputs some MIDI file with a simple
drum and bass track over these chords. The long term goal is a sophisticated high-level
composing environment, especially useful for creating demos for bands. You should be able to
make instructions like: Give me four bars of mainstream jazz over these chords, then
switch to heavy-metal, using this melody and these chords, afterwards a short drum break,
and so on.

Under Microsoft Windows there are a lot of different programs for music production systems
(Cubase, Band-In-A-Box, Finale). Instead of such WYSIWYG systems, the UNIX world has
traditionally used language-based approaches in various application areas (e.g. \LaTeX for
typesetting). The advantage of the first approach is that it is easier to learn, the
advantage of the second approach is that it is more flexible (and one can always add
a WYSIWYG interface afterwards). For this software we follow the second approach.

In the area of music various languages for representing and creating music have been
developed, see \cite{dannenberg:89}, \cite{collinge:84}, \cite{anderson:91} and
\cite{cointe:84} for just a few examples. Most of the existing systems provide very
general languages with an emphasis on gaining theorical insight, while the system, that is
presented here, should be \emph{practical} and \emph{useful}.

For writing the software, the library \texttt{Haskore} \cite{haskore} programmed in the
functional programming language \texttt{Haskell} (see \cite{haskell, hudak:96} for further
references) proved to be the perfect basis for such a system. Another author, Martin
Schwenke \cite{schwenke}, is working on a similar system, aimed at a slightly different
application area.

This program is free software; you can redistribute it and/or modify it under the terms of
the GNU General Public License as published by the Free Software Foundation; either
version 2 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
See the GNU General Public License for more details.

This document consists of a (short) user-manual, and the literate source code.



\section{User Manual}

The program acts as a filter, which takes some chord chart from standard input, and writes
the corresponding MIDI file to standard output. This output can be directly piped into
some MIDI player. Information about run-time options can be obtained by calling the
program with the \texttt{-h} option.

The syntax of input files is as follows:

\begin{haskelllisting}
  chart = { (bar | '%') '|' }

  bar = { chord | timeSig | '%' }

  timeSig = '(' int '/' int ')'

\end{haskelllisting}

Chords follow the usual syntax (e.g., like in the Real Book). The character \texttt{\%}
acts as a short-cut for repeating the last bar or chord, respectively. Examples of chord
charts come with the program distribution.

\section{Main Program}

We just extract the options from the command-line, and construct a string-to-string filter
from the chord-chart and options.

\begin{haskelllisting}

> module Main where
> import qualified Option
> import qualified Haskore.Interface.AutoTrack.Style as Style
> import qualified Data.ByteString.Lazy as B
> import Data.Char (chr, )

> main :: IO ()
> main =
>    do (t, s, r, c) <- Option.getAll
>       interact
>          (map (chr . fromIntegral) . B.unpack .
>           Style.playToStream r s t c . read)

\end{haskelllisting}

\input{ChartBar.lhs}
\input{ChordChart.lhs}
\input{EventChart.lhs}
\input{ScaleChart.lhs}

\input{Style.lhs}

\input{ChordSymbol.lhs}

\input{Instrument.lhs}

%\input{Scales.lhs}

%\input{Rhythm.lhs}

\input{Transposeable.lhs}

\input{Option.lhs}

\section{Todo}

\begin{itemize}
\item rock style: electric bass
\item humanize drums (tempo, single notes)
\item modularize styles, make style creation simpler
\item walking bass
\item recording music / reading in MIDI files
\item intros, codas, turnarounds etc.
\item breaks (e.g. night in tunesia), rhythmic accents
\item different styles within one theme (e.g., on green dolphin street)
\item error messages on wrong chord charts (for example takeFive not in 5/4 measure) (prelude function "error") via Monads!!!
\item more structured approach to parsing chord charts (either parsing tool/library, or
  via ReadS, or: treat EBNF rules as function definitions, EBNF operators as combinators);
  Even better: Try to get rid of a custom file format and to replace it by descriptions in pure Haskell code.
\item various degrees of shuffle
\end{itemize}

\bibliographystyle{abbrv}
\bibliography{composer}

\end{document}
