\paragraph{Naming Instruments and Tables}

In CSound, each table and instrument has a unique identifying integer
associated with it.  Haskore, on the other hand, uses strings to name
instruments.  What we need is a way to convert Haskore instrument
names to identifier integers that CSound can use.  Similar to
Haskore's player maps, we define a notion of a \keyword{CSound name map}
for this purpose.
\begin{haskelllisting}

> module Haskore.Interface.CSound.InstrumentMap where
>
> import Haskore.Interface.CSound (PField, Instrument, instruments)
>
> import qualified Data.List as List

> type SoundTable instr = [(instr, Instrument)]

\end{haskelllisting}
A name map can be provided directly in the form
\code{[("name1", int1), ("name2", int2), ...]}, or the programmer can
define auxiliary functions to make map construction easier.
For example:
\begin{haskelllisting}

> tableFromInstruments :: [instr] -> SoundTable instr
> tableFromInstruments nms = zip nms $ instruments

\end{haskelllisting}
The following function will add a name to an existing name map.
If the name is already in the map, an error results.
\begin{haskelllisting}

> addToTable :: (Eq instr) =>
>    instr -> Instrument -> SoundTable instr -> SoundTable instr
> addToTable nm i instrMap =
>    if elem nm (map fst instrMap)
>      then ((nm,i) : instrMap)
>      else (error ("CSound.addToTable: instrument already in the map"))

\end{haskelllisting}

Note the use of the function \function{lookup} imported from \module{List}.
\begin{haskelllisting}

> type ToSound instr = instr -> ([PField], Instrument)

> lookup :: (Eq instr) => SoundTable instr -> ToSound instr
> lookup table instr =
>    maybe (error "CSound.InstrMap.lookup: instrument not found")
>          ((,) [])
>          (List.lookup instr table)

\end{haskelllisting}
