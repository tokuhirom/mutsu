# `\c[NAME]` fails to resolve Unicode NameAlias corrections and multi-codepoint named sequences

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/unicode.rakudoc:190`, `:212`, `:224`).

## Root cause

`lookup_unicode_char_by_name` (`src/token_kind.rs`) resolves `\c[NAME]` via the
`unicode_names2` crate (single-codepoint standard `Name` property lookups) plus a small
hardcoded fallback table for C0 control names, and a separate `lookup_emoji_sequence`
(backed by the `emojis` crate) for CLDR emoji short names. This covers ordinary character
names and basic single-emoji CLDR names, but misses three related data sources real Raku's
`\c[...]` also consults:

1. **Unicode `NameAlias` corrections.** Some characters have a "correction" alias distinct
   from their immutable (never-changed) `Name` property — e.g. U+01A2's stable `Name` is
   `LATIN CAPITAL LETTER OI` (what `.uniname` reports), but its corrected alias
   `LATIN CAPITAL LETTER GHA` is also a valid `\c[...]` input. `unicode_names2` only
   indexes the primary `Name` property, not `NameAlias` corrections, so `\c[LATIN CAPITAL
   LETTER GHA]` (and other alias-only names) fail to resolve at all.
2. **Named character sequences** (`NamedSequences.txt`) — some `\c[NAME]` inputs name a
   *sequence* of codepoints (e.g. a base letter + combining diacritic), not a single
   character. mutsu's lookup only ever returns a single `char`, so these fail entirely.
3. **Compound/multi-emoji named sequences** beyond a single CLDR short name — e.g. a
   "family: man woman girl boy" ZWJ sequence with a colon-prefixed category. `emojis`
   crate's flat per-emoji name index apparently doesn't include these compound names (a
   single simpler CLDR name like "woman gesturing OK" does resolve correctly).

## Minimal repro

```raku
say "\c[LATIN CAPITAL LETTER GHA]";
say "\c[LATIN CAPITAL LETTER E WITH VERTICAL LINE BELOW AND ACUTE]".ords;
say "\c[PRESENTATION FORM FOR VERTICAL RIGHT WHITE LENTICULAR BRACKET]".ords;
say "\c[woman gesturing OK]".ords;
say "\c[family: man woman girl boy]".ords;
```

- `raku`:
  ```
  Ƣ
  (201 809)
  (65048)
  (128582 8205 9792 65039)
  (128104 8205 128105 8205 128103 8205 128102)
  ```
- `mutsu`:
  ```
  (empty line -- name not found, or renders as empty)
  ()
  ()
  (128582 8205 9792 65039)
  ()
  ```
  Only the plain single-emoji CLDR name ("woman gesturing OK") resolves correctly; the
  NameAlias-only name, the accented-letter named sequence, the corrected-spelling alias,
  and the compound family emoji sequence all fail.

## Affected files (starting point)

- `src/token_kind.rs` — `lookup_unicode_char_by_name` (single-char lookup, needs a
  `NameAlias`-correction fallback table or a crate/data source that includes them) and
  `lookup_emoji_sequence` (needs compound/colon-prefixed sequence names).
- Wherever `\c[...]` is parsed/evaluated (grep for `lookup_unicode_char_by_name` callers)
  — needs a new lookup path returning a `String`/multi-codepoint result for
  `NamedSequences.txt`-style entries, since the current API assumes a single `char`.
- Consider whether `unicode_names2`'s crate version/feature flags expose `NameAlias` data,
  or whether a small supplementary static table (like the existing C0-control fallback) is
  the pragmatic fix for the alias-correction cases.
