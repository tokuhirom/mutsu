# An underscore in a `<nu/de>` fraction's *numerator* should disqualify the literal reading

Originally filed as "`<1.5/2>` and friends: a quote-words fraction should divide two
*arbitrary* numerics, not just two integers". **Re-measured on `main` @ `17139dd55`
(2026-08-25): the entire arbitrary-numerics table now matches `raku` exactly** — all twelve
rows (decimals, `Num`s, `Inf`, `NaN`, signed denominators) produce the same `RatStr`/`NumStr`
values, and the zero-denominator cases `<1/0>` (plain `Rat`) and `< 1/0 >` (`RatStr`) both
still behave. That work is done; only the underscore sub-finding recorded at the bottom of the
old ticket survives, so the ticket is rescoped to it.

## Divergence

Rakudo's `bare_rat_number` lookahead makes an underscore in the **numerator** disqualify the
literal reading (so the word falls through to `val()` and becomes an allomorph), while an
underscore in the **denominator** does not.

| expression | raku | mutsu |
| --- | --- | --- |
| `<1_0/2>` | `RatStr.new(5.0, "1_0/2")` | `Rat` `5.0` |
| `<0x1_0/2>` | `RatStr.new(8.0, "0x1_0/2")` | `Rat` `8.0` |
| `<1/1_0>` | `Rat` `0.1` | `Rat` `0.1` (matches) |

mutsu's `is_angle_rat_literal()` strips underscores on **both** sides, so it reads the first two
as literals and loses the `Str` half of the allomorph.

## Repro

```raku
say <1_0/2>.raku;     # raku: RatStr.new(5.0, "1_0/2")   mutsu: 5.0
say <0x1_0/2>.raku;   # raku: RatStr.new(8.0, "0x1_0/2") mutsu: 8.0
say <1/1_0>.raku;     # both: 0.1
```

## Before implementing

The asymmetry is odd enough that it should be **confirmed against the current Rakudo grammar**
before being encoded — it may be an accident of `bare_rat_number`'s lookahead rather than an
intended rule. Read `Raku::Grammar`'s `bare_rat_number` / `rat_number` and check whether the
numerator-only underscore rejection is deliberate. If it is incidental, the right outcome may
be to leave mutsu as-is and close this, rather than reproducing a quirk.

## Affected files

- `src/parser/primary/container/allomorph.rs` — `is_angle_rat_literal()` (the underscore
  stripping), not `parse_angle_rat_word()` (which is now correct).
