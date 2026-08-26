# `\c[NAME]` resolves name aliases, named sequences and compound emoji names

`\c[...]` (and `uniparse` / `parse-names`, which share the lookup) went through
the `unicode_names2` crate plus a hand-written table of C0 control names. That
covers the immutable `Name` property and little else, so three whole UCD data
sources were missing.

## What was missing

1. **`NameAliases.txt`.** A character's `Name` is immutable, so the standard
   publishes *aliases* for the ones that were wrong or that need short forms.
   `unicode_names2` indexes only `Name`, so a corrected spelling
   (`LATIN CAPITAL LETTER GHA` for U+01A2, whose `Name` is still
   `LATIN CAPITAL LETTER OI`; `PRESENTATION FORM FOR VERTICAL RIGHT WHITE
   LENTICULAR BRACKET` for U+FE18, whose `Name` keeps the standard's famous
   `BRAKCET` typo) resolved to nothing at all. The hand-written table happened
   to cover the C0 controls and their abbreviations but not the C1 ones, not
   `NBSP`/`ZWJ`/`LRM`/`CGJ`/…, not `BYTE ORDER MARK`, and not `VS1`..`VS256`.

2. **`NamedSequences.txt`.** Some `\c[...]` inputs name a *sequence* of code
   points: `\c[LATIN CAPITAL LETTER E WITH VERTICAL LINE BELOW AND ACUTE]` is
   U+00C9 U+0329. mutsu's lookup returned a single `char`, so these could not
   be expressed.

3. **Compound CLDR emoji names.** `\c[family: man woman girl boy]` failed while
   `\c[woman gesturing OK]` worked. The `emojis` crate does have the ZWJ
   sequence, under its CLDR short name `family: man, woman, girl, boy` — with
   commas. But `\c[...]` and `uniparse` split their input on commas *before*
   the lookup (that is how `\c[A, B]` means two characters), so the name that
   arrives has already lost them. Rakudo accepts exactly the comma-less
   spelling for the same reason.

## The fix

Two generated tables, in the style of the existing `unicode_numval_table.rs`:

- `src/builtins/unicode_name_alias_table.rs` — 217 entries covering every alias
  kind (correction, control, abbreviation, figment, alternate). The 256
  `VS1`..`VS256` variation-selector abbreviations are an arithmetic series and
  are resolved by formula instead of costing 256 rows.
- `src/builtins/unicode_named_sequence_table.rs` — the 461 named character
  sequences, returning a `&'static str`.

`lookup_unicode_char_by_name` consults the alias table after `unicode_names2`
(the hand-written C0 table stays as a last resort for the few non-UCD synonyms
it carries). A new `lookup_unicode_name_string` layers single character →
named sequence → emoji sequence, and both `\c[...]` call sites and `uniparse`
now go through it — including the per-comma-part path, so a named sequence can
be one element of a multi-name `\c[A, B]`. The emoji comparison additionally
matches with commas removed on both sides, which is what recovers the compound
family/couple names.

All five of the ticket's repro lines, plus `BYTE ORDER MARK`, `VS16`,
`HIGH OCTET PRESET`, `WEIERSTRASS ELLIPTIC FUNCTION`, `KEYCAP NUMBER SIGN` and
a named sequence used as one comma-separated part, now match `raku` v2026.06
exactly. Pinned by `t/regex-engine-gaps.t`.

The tables were generated from the normative UCD contributory files
(`NameAliases.txt`, `NamedSequences.txt`, Unicode 15.1). Regenerating them for
a newer Unicode release is a mechanical re-run over those two files.
