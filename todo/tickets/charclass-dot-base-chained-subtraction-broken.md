# Regex character class with a `.` (any-char) base and two chained `-` subtractions is mis-parsed

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Test.rakudoc:323`).

## Root cause

A Raku regex character class of the shape `<.-A-B>` — the any-char base `.` followed by
**two** chained subtraction parts — is not composed correctly. A *single* subtraction
after the dot base works fine (`<.-:letter>` correctly matches "any char that is not a
letter"), but adding a second subtraction breaks differently depending on the item kind:

- Two Unicode-property subtractions (`<.-:letter-:digit>`) silently **drops both
  subtractions** and matches every character (equivalent to bare `.`).
- Two bracket-class subtractions (`<.-[a]-[b]>`) does the opposite: it becomes
  **over-restrictive** and matches nothing.

Plain (non-dot-based) chained subtraction works correctly in both cases
(`<-:letter-:digit>`, `<-[a]-[b]>`), so the bug is specifically in how the leading `.`
combines with a *chain* of two or more subtraction parts. `parse_combined_class` in
`src/runtime/regex_parse_charclass.rs` (the `+`/`-` prefixed-part loop starting around
line 749) is the composition logic for chained parts; the dot-base case likely enters a
different code path upstream (in `src/runtime/regex_parse_core.rs`, the `<...>` content
dispatch) that does not fully delegate to the same chained-subtraction accumulator once a
leading `.` is present.

## Minimal repro

```raku
say "ab1 c".comb(/<.-:letter-:digit>/);   # any char, not letter, not digit
say "ab1 c".comb(/<.-[a]-[b]>/);          # any char, not 'a', not 'b'
```

- `raku`: `( )` (a single space) for the first; `(1   c)` (digit, space, 'c') for the second.
- `mutsu` (`target/debug/mutsu`): `(a b 1  c)` (everything — subtraction ignored) for the
  first; `()` (nothing — over-restricted) for the second.

Single-subtraction dot-base forms are unaffected and already correct:

```raku
say "ab1 ".comb(/<.-:letter>/);   # both raku and mutsu: (1  )
```

This is what the doc example (`Type/Test.rakudoc:323`) actually exercises via
`.comb(/<.-:letter-:digit>/)` inside a `Bag.new-from-pairs` count, producing a wrong
`other` count (21, the whole string length, instead of 4).

## Affected files (starting point)

- `src/runtime/regex_parse_charclass.rs` (`parse_combined_class`, the chained `+`/`-`
  accumulator)
- `src/runtime/regex_parse_core.rs` (the `<...>` dispatch that decides when to route into
  `parse_combined_class` vs. some other any-char/dot-specific path)
