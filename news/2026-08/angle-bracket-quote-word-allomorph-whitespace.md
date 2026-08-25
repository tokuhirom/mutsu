# `< 42/10 >` keeps its `RatStr` allomorph: one allomorph builder plus a literal-term predicate

`< 42/10 >` reported a plain `Rat` where Raku gives a `RatStr`. Fixing it turned out to be less
about whitespace than about a rule mutsu had encoded backwards, so the fix replaced the special
case with the actual grammar distinction and picked up three more divergences on the way.

## The rule

`<...>` is fundamentally a **quote-words** construct, and quote-words *always* produce the
allomorph (`IntStr` / `RatStr` / `NumStr` / `ComplexStr`) for a number-shaped word. Raku's plain
`Rat` and `Complex` do not come from quote-words at all — they come from two separate numeric
**literal terms** in the grammar, `rat_number` (`<nu/de>`) and `complex_number` (`<re±im i>`),
which are recognised only when the bracket content is *exactly* that literal. Padding whitespace
disqualifies the literal reading, which is why the same number changes type:

```raku
<42/10>    # Rat        < 42/10 >    # RatStr
<1+42i>    # Complex    < 1+42i >    # ComplexStr
```

mutsu had this inverted. It carried a `fraction_allomorphic` flag through the word-value builder:
`false` produced a bare `Rat` for a fraction and was used for the single-word `<...>` path, `true`
produced the allomorph and was used for word lists. The `Complex` half of the same function was
already written the other way round — always build the allomorph, then strip it back for the tight
case — so the two shapes disagreed, and only `Complex` handled padding correctly.

## The fix

The `fraction_allomorphic` flag is gone. `angle_word_value()` now has exactly one behaviour: build
the allomorph. The literal decision moved out of the value builder and into a new syntactic
predicate, `angle_word_is_numeric_literal()`, which the `<...>`-as-a-term parser consults to unwrap
the allomorph for a genuine `rat_number` / `complex_number` term. One builder, one predicate,
applied uniformly to `Rat` and `Complex` instead of two disagreeing special cases.

Collapsing the flag also fixed a caller that had quietly been on the wrong side of it. A colonpair
value is a quote-words slot, never a term, so `:a<2/3>` is a `RatStr` in Raku — but `colonpair.rs`
called the non-allomorphic variant and returned a plain `Rat`. With only one builder left, it is
correct by construction.

Three further divergences fell out of making the predicate faithful rather than approximate:

- **A bare imaginary is not a complex literal.** `complex_number` needs both a real and an
  imaginary part, so `<42i>`, `<+42i>` and `<-42i>` stay `ComplexStr` even when tight. mutsu had
  been stripping any `Complex` to a plain one. `parse_angle_complex` now reports whether a real
  part was present, which is exactly the distinction the predicate needs.
- **A signed denominator is not a rat literal.** `bare_rat_number` is `signed-integer '/' integer`,
  so `<+1/2>` is a plain `Rat` but `<1/+3>` is a `RatStr`.
- **`Inf` and `NaN` are Num-shaped words**, so they are `NumStr` allomorphs; mutsu returned a plain
  `Str`. The spellings are case-sensitive and only `Inf` takes a sign, so `<Inf>`, `<+Inf>`,
  `<-Inf>` and `<NaN>` are `NumStr` while `<inf>`, `<nan>` and `<-NaN>` remain `Str`.

U+2212 MINUS SIGN is normalized inside the predicate, keeping the `roast/S02-literals/allomorphic.t`
pins on `<5−1i>` and `<−5−1i>` as plain `Complex`.

## Verification

`t/allomorph-angle-bracket-whitespace.t` pins the whole matrix — Int, Rat (decimal and fraction),
Num, `Inf`/`NaN`, Complex (full form and bare imaginary), radix and signed forms, U+2212, word
lists, colonpair values and non-numeric words — tight and space-padded for each. All 71 assertions
were run against real `raku` first and pass there unchanged, so the file is a spec oracle rather
than a description of mutsu's behaviour.

## Deferred

Two adjacent findings were split out rather than bundled in, since neither is part of the
literal-vs-quote-words decision:

- `todo/tickets/angle-bracket-fraction-underscore-numerator-asymmetry.md` — a quote-words
  fraction should divide two *arbitrary* numerics (`<1.5/2>`, `<Inf/2>`, `<1/-3>` are allomorphs in
  Raku but plain `Str` in mutsu, because the parser only accepts `integer/integer`). *(That part
  has since been fixed; the ticket was renamed on 2026-08-25 and now tracks only the leftover
  underscore-in-numerator asymmetry.)*
- `todo/tickets/allomorph-raku-repr-loses-zero-denominator-rat.md` — `RatStr.new(1/0, "1/0").raku`
  renders the numeric half as `Inf`. Display-only and pre-existing (it reproduces with no `<...>`
  involved); the whitespace fix merely made it reachable from `< 1/0 >`.
