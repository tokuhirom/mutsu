# `.uniname` returns `<reserved-XXXX>` for surrogates, private-use and Tangut codepoints

Found while investigating `todo/perf/sort-key-extraction-per-element-closure-call.md`:
the perf repro summed `.uniname.chars` over the whole Unicode range and mutsu's
total (2797034) disagreed with raku's (2855332).

A full `0..0x10FFFF` diff of `$cp.uniname` between mutsu and raku shows **145713
divergent codepoints in exactly four families** — every other algorithmic name
family (Hangul syllables, Nushu, Khitan, Egyptian Hieroglyph, CJK Compatibility,
Tangut Component) already matches, because `unicode_names2` covers them.

| raku | mutsu | ranges | count |
|---|---|---|---|
| `<private-use-XXXX>` | `<reserved-XXXX>` | `E000..F8FF`, `F0000..FFFFD`, `100000..10FFFD` | 137468 |
| `TANGUT IDEOGRAPH-XXXXX` | `<reserved-XXXXX>` | `17000..187FF`, `18D00..18D1E` | 6175 |
| `<surrogate-XXXX>` | `<reserved-XXXX>` | `D800..DFFF` | 2048 |
| `CJK UNIFIED IDEOGRAPH-XXXXX` | `<reserved-XXXXX>` | `2B81E..2B81F`, `2EBE1..2EBEF`, `3134B..3134F` | 22 |

## Root cause

`builtins/unicode.rs::unicode_char_name_by_codepoint` handles noncharacters and
controls explicitly, then defers to `unicode_names2::name`, and calls everything
else `<reserved-XXXX>`. It has no notion of the *surrogate* or *private-use*
sentinels, and no algorithmically-derived name ranges of its own — so it inherits
whatever `unicode_names2`'s (older) UCD snapshot happens to contain. MoarVM
derives all of these from ranged-name tables instead, which is why raku is
complete.

The last row is just UCD drift: raku's CJK Unified ranges are
`3400..4DBF`, `4E00..9FFF`, `20000..2A6DF`, `2A700..2CEAD`, `2CEB0..2EE5D`,
`30000..33479`; `unicode_names2` stops slightly short of three of them.

## Fix sketch

Handle the ranged/sentinel families in `unicode_char_name_by_codepoint` *before*
consulting `unicode_names2`:

- `D800..DFFF` → `<surrogate-XXXX>` (note `char::from_u32` returns `None` here,
  so this must be checked before the `if let Some(ch)` branch).
- `E000..F8FF`, `F0000..FFFFD`, `100000..10FFFD` → `<private-use-XXXX>`.
- the six CJK Unified ranges → `CJK UNIFIED IDEOGRAPH-{:X}`.
- `17000..187FF`, `18D00..18D1E` → `TANGUT IDEOGRAPH-{:X}`.

`uniparse` needs the matching inverse for the two *name* families — it is
currently asymmetric: `uniparse('TANGUT IDEOGRAPH-17000')` fails with
"Unrecognized character name" while `uniparse('CJK UNIFIED IDEOGRAPH-4E00')`
works (via `unicode_names2::character`). Add the range parse in
`token_kind.rs::lookup_unicode_char_by_name`.

## Verification

Diff the whole range against raku — it should come back empty:

```
for 0..0x10FFFF -> $cp { say "$cp\t{$cp.uniname}" }
```

## Affected files

- `src/builtins/unicode.rs` — `unicode_char_name_by_codepoint`.
- `src/token_kind.rs` — `lookup_unicode_char_by_name` (the `uniparse` inverse).
