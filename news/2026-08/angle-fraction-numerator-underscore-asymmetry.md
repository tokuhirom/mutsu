# Angle-bracket fraction literals: underscore in the numerator disqualifies the plain-`Rat` reading

`<nu/de>` inside angle brackets can read either as Raku's `rat_number` literal
term (a plain `Rat`) or as ordinary quote-words (a `RatStr` allomorph),
depending on the exact shape of the word. mutsu already matched Rakudo on the
full arbitrary-numerics table (decimals, `Num`s, `Inf`, `NaN`, signed
denominators, zero denominators), but had one surviving asymmetry: an
underscore in the numerator should disqualify the literal reading while an
underscore in the denominator should not.

```raku
say <1_0/2>.raku;   # RatStr.new(5.0, "1_0/2")
say <1/1_0>.raku;   # Rat  0.1
```

mutsu's `is_angle_rat_literal()` (`src/parser/primary/container/allomorph.rs`)
stripped underscores permissively on both sides, so it read both forms as
plain `Rat` literals, losing the `Str` half of the allomorph for the
numerator case.

## Confirming this is a real rule, not an accident

Rakudo's grammar (`src/Perl6/Grammar.nqp`) defines:

```
token bare_rat_number { <?before <.[-−+0..9<>:boxd]>+? '/'> <nu=.signed-integer> '/' <de=integer> }
```

The `<?before ...>` lookahead only scans the numerator, up to the first `/`,
through a character class (`-−+0-9<>:bodx`) that does not include `_`. Since
the lookahead requires that whole numerator-prefix to match the class
contiguously, any underscore in the numerator breaks the run and the
lookahead fails outright, disqualifying `bare_rat_number` entirely -- the
word falls through to the generic allomorph path. An underscore anywhere
*after* the first `/` (i.e. in the denominator) is invisible to this
lookahead, since it stops scanning at the slash.

The same character class (still without `_`) appears in the sibling
`bare_complex_number` production's lookahead, which rules out "this specific
underscore omission was a one-off typo" -- it is a systematic feature of how
these disambiguating lookaheads are written in the shipped grammar, and
therefore a genuine, reproducible piece of current Rakudo behavior rather
than build-to-build noise.

A boundary probe against the installed `raku` (2026.06) confirms a clean
numerator/denominator split, entirely independent of radix prefix or sign:

| expression | raku result |
| --- | --- |
| `<1_0/2>` | `RatStr.new(5.0, "1_0/2")` |
| `<10/2_0>` | `Rat` `0.5` |
| `<1_0/2_0>` | `RatStr.new(0.5, "1_0/2_0")` |
| `<0x1_0/2>` | `RatStr.new(8.0, "0x1_0/2")` |
| `<0b1_0/2>` | `RatStr.new(1.0, "0b1_0/2")` |
| `<-1_0/2>` | `RatStr.new(-5.0, "-1_0/2")` |
| `<1/1_0>` | `Rat` `0.1` |
| `<0x_2a/2>` | `RatStr.new(21.0, "0x_2a/2")` (isolated underscore right after a radix prefix still counts) |
| `<_10/2>` | `"_10/2"` (plain `Str` -- leading underscore is not even a valid numeral) |
| `<10_/2>` | `"10_/2"` (plain `Str` -- trailing underscore) |
| `<1__0/2>` | `"1__0/2"` (plain `Str` -- doubled underscore) |
| `<1_0e0/2>` | `NumStr.new(5e0, "1_0e0/2")` (already matched -- not a plain integer numerator) |
| `<1_0.5/2>` | `RatStr.new(5.25, "1_0.5/2")` (already matched) |

While investigating this, we found the fraction-literal path in
`allomorph.rs` (`parse_angle_int` / `parse_angle_bigint`, used only by
`is_angle_rat_literal()` and `parse_angle_rat_word()`) never validated that
underscores were "isolated" (Raku's `decint`/`hexint` productions are all
`[\d+]+ % '_'`: no leading/trailing underscore, no doubled underscore) --
it simply stripped every underscore before parsing. That made `<_10/2>`,
`<10_/2>`, and `<1__0/2>` parse as numeric (`RatStr`) in mutsu when Raku
treats them as invalid numerals and leaves the whole word a plain `Str`.
mutsu's general `val()` builtin and its main integer-literal parser already
enforce this correctly (verified directly); the gap was narrowly confined
to this one fraction-literal helper.

## Fix

- `is_angle_rat_literal()` now rejects the literal (bare `Rat`) reading
  whenever the numerator contains `_`, regardless of radix prefix or sign,
  matching the lookahead's numerator-only, underscore-excluding character
  class.
- `parse_angle_int()` / `parse_angle_bigint()` (the fraction-only integer
  parsers) now reject a leading, trailing, or doubled underscore via a new
  `has_isolated_underscores()` helper, so malformed numerals like `_10` or
  `10_` fall all the way through to plain `Str` instead of being silently
  accepted as `RatStr`.

13 new assertions were added to `t/allomorph-angle-bracket-whitespace.t`
covering the full boundary table above; all pass under both `raku` and
mutsu.

Affected file: `src/parser/primary/container/allomorph.rs`.
