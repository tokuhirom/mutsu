# `<1.5/2>` and friends: a quote-words fraction should divide two *arbitrary* numerics, not just two integers

Found while fixing `todo/tickets/angle-bracket-quoted-word-space-padded-loses-allomorph.md`
(see `news/2026-08/angle-bracket-quote-word-allomorph-whitespace.md`). That fix corrected the
*literal-vs-quote-words* decision; this ticket is about the remaining gap in the **value** a
number-shaped quote-word parses to.

## Root cause

`parse_angle_rat_word()` in `src/parser/primary/container/allomorph.rs` only accepts
`integer '/' integer` (via `parse_angle_int` / `parse_angle_bigint`), and bails out entirely on a
negative denominator. Rakudo's `val()` instead parses each side as an arbitrary numeric and
divides them, so the numerator and denominator may be decimals, `Num`s, `Inf`/`NaN`, or signed.
When mutsu's narrow parser returns `None` the word falls all the way through to a plain `Str`.

The result type follows ordinary Raku division: `Rat / Rat` stays a `RatStr`, and a `Num` on
either side makes it a `NumStr`.

## Divergences (measured 2026-08-23, `raku` vs `target/debug/mutsu`)

| expression | raku | mutsu |
| --- | --- | --- |
| `<1.5/2>` | `RatStr.new(0.75, "1.5/2")` | `Str` |
| `<1/2.5>` | `RatStr.new(0.4, "1/2.5")` | `Str` |
| `<.5/2>` | `RatStr.new(0.25, ".5/2")` | `Str` |
| `<1/.5>` | `RatStr.new(2.0, "1/.5")` | `Str` |
| `<1/-3>` | `RatStr.new(<-1/3>, "1/-3")` | `Str` |
| `<-1/-3>` | `RatStr.new(<1/3>, "-1/-3")` | `Str` |
| `<1e2/2>` | `NumStr.new(50e0, "1e2/2")` | `Str` |
| `<2/1e2>` | `NumStr.new(0.02e0, "2/1e2")` | `Str` |
| `<1.5e2/2>` | `NumStr.new(75e0, "1.5e2/2")` | `Str` |
| `<Inf/2>` | `NumStr.new(Inf, "Inf/2")` | `Str` |
| `<2/Inf>` | `NumStr.new(0e0, "2/Inf")` | `Str` |
| `<NaN/2>` | `NumStr.new(NaN, "NaN/2")` | `Str` |

None of these are *literal* terms, so all of them are allomorphs in Raku — the fix is purely in
the quote-word value parser, and it cannot change any `<nu/de>` plain-`Rat` result.

## Why it was not done in the whitespace fix

It needs division performed at **parse** time with Raku's Rat/Num promotion rules, including the
zero-denominator case that must *not* throw (`<1/0>` is a `Rat` with denominator 0, and
`< 1/0 >` a `RatStr` wrapping one). The existing integer path reaches `make_rat()`, which already
handles `1/0`; a generalized path has to preserve that while adding Num promotion. That is a
distinct piece of work from the syntactic literal-vs-quote-words decision, so it was split out
rather than bundled in.

## Minimal repro

```raku
say <1.5/2>.^name;   # raku: RatStr    mutsu: Str
say <Inf/2>.^name;   # raku: NumStr    mutsu: Str
```

## Affected files

- `src/parser/primary/container/allomorph.rs` — `parse_angle_rat_word()`

## Related, smaller divergence: underscores in a fraction literal

Rakudo's `bare_rat_number` lookahead makes an underscore in the **numerator** disqualify the
literal reading, while an underscore in the denominator does not:

| expression | raku | mutsu |
| --- | --- | --- |
| `<1_0/2>` | `RatStr.new(5.0, "1_0/2")` | `Rat` `5.0` |
| `<0x1_0/2>` | `RatStr.new(8.0, "0x1_0/2")` | `Rat` `8.0` |
| `<1/1_0>` | `Rat` `0.1` | `Rat` `0.1` (matches) |

mutsu's `is_angle_rat_literal()` strips underscores on both sides, so it reads the first two as
literals. The asymmetry is odd enough that it should be confirmed against the current Rakudo
grammar before being encoded; it is noted here so the observation is not lost.
