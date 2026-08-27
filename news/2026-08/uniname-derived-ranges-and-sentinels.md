# `.uniname` now matches raku on every one of the 1,114,112 codepoints

`.uniname` returned `<reserved-XXXX>` for 145713 codepoints that raku names
properly — 13% of the Unicode space. Found while investigating a perf ticket
(`todo/perf/sort-key-extraction-per-element-closure-call.md`): the repro summed
`.uniname.chars` over `0..0x1FFFF` and mutsu's total (2797034) disagreed with
raku's (2855332), which turned a "we are slow here" report into a correctness
finding.

A full `0..0x10FFFF` diff against `raku` isolated exactly four families:

| raku | mutsu | ranges | count |
|---|---|---|---|
| `<private-use-XXXX>` | `<reserved-XXXX>` | `E000..F8FF`, `F0000..FFFFD`, `100000..10FFFD` | 137468 |
| `TANGUT IDEOGRAPH-XXXXX` | `<reserved-XXXXX>` | `17000..187FF`, `18D00..18D1E` | 6175 |
| `<surrogate-XXXX>` | `<reserved-XXXX>` | `D800..DFFF` | 2048 |
| `CJK UNIFIED IDEOGRAPH-XXXXX` | `<reserved-XXXXX>` | `2B81E..2B81F`, `2EBE1..2EBEF`, `3134B..3134F` | 22 |

## Root cause

`unicode_char_name_by_codepoint` handled noncharacters and controls explicitly
and deferred everything else to `unicode_names2::name`, calling any miss
`<reserved-XXXX>`. Two consequences:

- It had no notion of the **surrogate** or **private-use** sentinels. Surrogates
  were doubly hidden: `char::from_u32` rejects them, so they fell straight into
  the "not a valid char" arm.
- For **algorithmically-derived** names (`<prefix>-<hex>`, which Rakudo/MoarVM
  compute from ranged-name tables rather than storing) it inherited whatever
  `unicode_names2`'s older UCD snapshot happened to enumerate. `TANGUT
  IDEOGRAPH-*` was absent entirely, and three CJK Unified ranges stopped a
  handful of codepoints short of Rakudo's — pure UCD drift.

## Fix

`DERIVED_NAME_RANGES` in `builtins/unicode.rs` now derives the ranged names
mutsu was missing, consulted after the name table so it never shadows a real
name; the surrogate and private-use sentinels are checked up front, before the
`char::from_u32` conversion that would drop surrogates. Only the four diverging
families are listed — Hangul syllables, Nushu, Khitan Small Script, Egyptian
Hieroglyph, CJK Compatibility Ideograph and Tangut Component already agreed
through `unicode_names2`, and duplicating them would add a second, easily stale
source of truth for no gain.

`uniparse` / `\c[...]` gained the matching inverse. It had been asymmetric in a
way that is hard to defend: `uniparse('CJK UNIFIED IDEOGRAPH-4E00')` worked
(the name table has it) while `uniparse('TANGUT IDEOGRAPH-17000')` — a name
`.uniname` itself produces — raised "Unrecognized character name".

One Rakudo quirk is reproduced deliberately. In the `uniparse` direction Rakudo
parses three prefixes — `CJK UNIFIED IDEOGRAPH`, `CJK COMPATIBILITY IDEOGRAPH`
and `TANGUT IDEOGRAPH` — with **no range check**, so
`uniparse('TANGUT IDEOGRAPH-99999')` returns U+99999 even though its `.uniname`
is `<reserved-99999>`. Every other derived family *is* range-checked
(`TANGUT COMPONENT-99999` and friends are rejected). That split was established
prefix by prefix against `raku` and is encoded as `UNRANGED_NAME_PREFIXES`.

## Verification

A full `0..0x10FFFF` dump of `$cp.uniname` is now **byte-identical to raku**, as
is the `uniparse` round-trip probe. Pinned by `t/uniname-derived-ranges.t` (30
tests at every range boundary, validated against `raku` first). The `t/` suite
(3499 files) and a 188-file whitelisted roast sweep of `S15-*` / `S32-str/*` /
`S05-mass/*` / `S02-literals/*` pass.
