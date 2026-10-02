# `.List`/`.Array` keep a `.lazy` map/grep lazy even when finite

`(1..3).lazy.map(* + 1).List` printed `(2 3 4)` and `.grep(...).Array`
printed `[2 3]`; Rakudo prints `(...)` and `[...]`. A map/grep over an
explicitly `.lazy` list builds a lazy pipe stage that carries the `lazy`
marker, but the laziness-preserving coercions (`.List`, `.Array`, `.list`,
`.values`, `.cache`) decided by *finiteness* and reified any pipe whose source
was finite.

They now ask `LazyList::coercion_keeps_pipe_lazy`: a pipe stays lazy when its
source is infinite **or** it is `lazy`-marked. An unmarked finite pipe, such as
`gather { ... }.grep(...)`, is not `.is-lazy` and still reifies.
