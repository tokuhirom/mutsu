# `Str.Numeric` rejects a lone `i`, as Rakudo does

Numifying a string whose imaginary part has no coefficient — `"i"`, `"+i"`,
`"3-i"`, `"\i"` — now fails with `X::Str::Numeric`, matching every released
Rakudo. Roast's `S32-str/numeric.t` asks for `+'−i'` to be `-i`, but fences
those assertions with `#?rakudo skip 'cannot handle lone i yet'`; real code
depends on Rakudo's behaviour. `Text::SubParsers` scans text with
`{ .trim.Numeric }` as a predicate, and under mutsu the `i` in words like "is"
and "with" numified into spurious `<0+1i>` pieces (#9731).

The same pass fixed the neighbouring divergences:

- `"Infi"` / `"NaNi"` (no backslash) and a doubled imaginary sign
  (`"0--1i"`, `"0+-Inf\i"`) no longer numify; `"NaN+0i"` now does.
- The `<...>` quote-words allomorph parser used Rust's `f64::parse`, which
  accepts `inf`/`nan` in any case, so `<infi>` and `<Infi>` were
  `ComplexStr`s while `<Inf\i>` was a plain `Str`. It now shares the
  `Str.Numeric` complex parser, and `<3+Inf\i>` is a `Complex` literal term.
- `Complex cmp Str` compared numerically; like Rakudo it now compares the
  two as strings (`"0+NaNi" cmp 0i` is `More`).
