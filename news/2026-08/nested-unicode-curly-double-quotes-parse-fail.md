# Unicode curly quote pairs now nest

`say “here: “no problem” at all!”;` — the "You can nest them!" example in
`raku-doc/doc/Language/unicode_entry.rakudoc` — printed
`here: “no problem” at all!` in rakudo, while mutsu reported
`Unable to parse expression in curly double quotes; couldn't find final '”'`.

## Root cause

`smart_double_quoted_string` and `smart_single_quoted_string`
(`src/parser/primary/string/quoted.rs`) scanned forward for the first character
in the closer set, with no nesting depth. mutsu already tracks depth for the
corner-bracket form `｢…｣` and for bracket delimiters used with `q(...)`; the
smart-quote path simply never did.

Probing `raku` v2026.06 gives the exact rule: depth is counted on the **opening**
character only. `“…”` and `‘…’` nest on `“`/`‘`; `„…”` (whose closer set is
`”`/`“`) nests on `„`; and the reversed-direction spellings `”…”` / `’…’`, where
the opener is itself a closer, do **not** nest — like `/…/`, any occurrence of
that character closes the string. An unbalanced inner opener (`“a “ b”`) is an
error in rakudo, which depth tracking produces naturally.

## Fix

Both parsers now carry a depth counter, gated by `quote_pair_nests(opener,
closers)` — literally `!closers.contains(&opener)`, which captures the
"reversed pairs do not nest" case without naming any character. An inner closer
below depth zero is literal text; only the one that brings the depth back to zero
ends the string. Escapes are consumed before the character is examined, so `\“`
still does not count toward depth.

All eight Unicode quote-pair behaviours checked against rakudo now agree,
including interpolation inside a nested quote (`“a “$z” b”`). Pinned by
`t/custom-operator-and-term-parsing.t` section 2.
