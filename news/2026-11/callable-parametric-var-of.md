# An unassigned `Callable[T]` variable keeps its parameter for `.of`

`my Callable[Int] $x;` and `my Int &f;` now hold the `Callable[Int]` type
object itself instead of the bare `Callable`, so `.of` answers `Int` (and
`Int:D` for a smiley) as in rakudo (#16449).
