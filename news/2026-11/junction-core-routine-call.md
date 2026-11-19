# A core routine called with a Junction argument autothreads

`abs(any(-1, -2))` died with "Unknown function: abs" and `chr(any(65, 66))`
likewise, while `uc(any("a", "b"))`, `sqrt(any(4, 9))` and
`flip(any("ab", "cd"))` took the Junction as a plain value (`ANY(A, B)`,
`NaN`, `)dc ,ba(yna`). Rakudo threads a Junction argument whenever no
candidate of the routine takes it whole, and answers `any(1, 2)`,
`any(A, B)`, `any(2, 3)`, `any(ba, dc)`.

The by-name autothreader found no signature for a built-in and declined. It
now asks the snapshot of Rakudo's core candidates (`builtins::rakudo_candidates`):
a routine none of whose candidates declares a `Mu` or `Junction` parameter, a
slurpy or a capture threads its Junction arguments like a user routine with
`Any` parameters (#18098).
