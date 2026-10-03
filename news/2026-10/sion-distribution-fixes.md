# SION distribution: five interpreter fixes

Working the `SION` zef distribution (a pure-Raku serialization format) from red to green; all four
of its test files now match rakudo (3, 71, 56 and 20 assertions). Each gap was a general bug:

- **Regex alternation lost to a shorter branch.** The LTM prefix measure caps `** m..n` at `m+1`
  copies, but the capped path then continued into the next atom, so
  `[ u '{' (<[0-9A-F]> ** 1..6) '}' | . ]` measured the first branch as 0 and chose `.`. A capped
  quantifier now ends the declarative prefix in a fate.
- **Native `int` `+<` wraps** at 64 bits like `+`, `-` and `*` (it promoted to a bigint and then
  died with "Cannot unbox 65 bit wide bigint").
- **Multi-method ranking sees through containers.** A hash element or `Pair.value` was ranked by the
  container, so `Date:D` lost to `Any:D` and tied with `Mu:D` (`X::Multi::Ambiguous`).
- **`FatRat.Num` / `Num(FatRat)` of a huge denominator** converts with one correctly rounded
  quotient (`FatRat.new(1, 2**1074).Num` is `5e-324`, was `0`); `Num(FatRat)` returned `0`.
- **`m:p(N)` / `m:c(N)` count graphemes**, and their Match reports grapheme `.from`/`.to`
  (`"\r\n"` is one character), as the ordinary match path already did.
