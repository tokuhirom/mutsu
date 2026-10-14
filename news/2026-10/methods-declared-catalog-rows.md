# `.^methods` lists more of the methods Rakudo declares on collections

`Pair`, `Set`/`Bag`/`Mix` (and their `*Hash` forms), `Map`, `List`, `Seq` and `Array` gained the
catalog rows for the methods Rakudo declares on them and mutsu already serves (`iterator`, `new`,
`deepmap`, `minpairs`/`maxpairs`, `sort`, `eager`, `rotate`, `Pair.freeze`). `Pair.^methods` went
from 22 to 25 entries, `Map` from 37 to 40, `Set` from 39 to 44 (part of #13494). The rows Rakudo
declares but mutsu does not implement (`Pair.abs-value`, `add-value`, `FLATTENABLE_*`, ...) stay
missing rather than being listed without a body.
