# deepmap/duckmap on a single item answer a list; flatmap flattens deeply

`42.deepmap(* + 1)` and `42.duckmap(...)` answered the bare value; Rakudo's `Any.deepmap` runs on
`self.list`, so the answer is a one-item list `(43)`. `.flatmap` flattened a mapper result only
one level, leaving `(1, (2, 3)).flatmap({ ($_, $_) })` with two nested lists; it is now
`.map(...).flat` as in Rakudo. The user-iterator receiver part of #13690 stays open.
