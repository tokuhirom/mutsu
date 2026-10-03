# Terminal::UI: sigilless `self` argument, allomorph subscripts and slice stores

Working the `Terminal::UI` distribution (ecosystem roulette) exposed four general interpreter
gaps, all now fixed:

- A sigilless (`\ui`) parameter bound to the caller's `self` recorded `self` as a writable alias
  source, so the callee's own `self` resolved to the caller's invocant (`No such private method`).
- `@a[$r][1..2] = ...` (a Range as the last subscript of a nested element) silently dropped the
  store.
- A positional subscript given an allomorph (`@a[<1>]`, `:top<1>` stored as an `IntStr`) read
  `Nil`, which also made `@a[$r] //= []` clobber an existing row.
- `Any[1..3]` (range slice of a type object) answered a single `Any` instead of a list, and a
  slice assignment stored `Nil` instead of resetting the slot to its default.

Under mutsu 8 of the 10 `Terminal::UI` test files now pass (was 3).

Also fixed: postfix/prefix `++`/`--` on a subscripted element (`@$h[@i[$++]]++`) evaluated a
non-trivial subscript twice (once for the read, once for the write-back). Terminal::UI now passes
9 of 10 files; `t/09-print` needs #11275.
