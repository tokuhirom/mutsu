# `\qq[...]` / `\q[...]` inside a `"..."` or `qq//` string is rejected

Found while making the `s///` replacement a real `qq` quote
(`news/2026-08/subst-replacement-is-a-qq-quote.md`) — the replacement inherited
this pre-existing `qq` gap along with everything else, so it is now visible in
substitutions too.

## Repro

```raku
say "a\qq[1+1]b";
say qq!a\qq[1+1]b!;
my $s = 'ab'; $s ~~ s/a/x\qq[1+1]y/; say $s;
```

- raku: `a1+1b` / `a1+1b` / `x1+1yb` — `\qq[...]` re-quotes its body under `qq`
  rules and splices the result in (`Language/quoting.rakudoc`, "Escaping" /
  `\q`, `\qq`).
- mutsu: `Unrecognized backslash sequence: '\q'` in all three.

`\qw[...]` and `\qqw[...]` *are* handled, but only by
`interp_content::parse_single_quote_qq` (the `q//`-with-adverbs path), not by
the `"..."` parser or by `process_content_with_flags`, which is what `qq//`,
heredocs and now substitution replacements use.

## Why it isn't a one-liner

There are three separate implementations of the interpolation walk in
`src/parser/primary/`: `string/quoted.rs::double_quoted_string` (for `"..."`),
`string/interp_content.rs::interpolate_string_content_with_modes`, and
`quote_adverbs.rs::process_content_with_flags` (for `q`/`qq` with adverbs, and
now for heredocs and `s///` replacements via `parser::interpolate_qq_content`).
They already disagree in small ways — the multi-statement `{ ... }` block was
fixed in exactly one of them and had to be shared across; `\qw` lives in a
fourth. Adding `\q`/`\qq` handling to whichever one is convenient would deepen
that divergence.

The right shape is to collapse them onto one walk (the way the substitution
replacement was collapsed onto `interpolate_qq_content`), then add the escape
once. That is the real unit of work here.

## Affected files

- `src/parser/primary/quote_adverbs.rs` — `process_content_with_flags`
- `src/parser/primary/string/quoted.rs` — `double_quoted_string`
- `src/parser/primary/string/interp_content.rs` — `try_embedded_qw`,
  `parse_single_quote_qq` (the existing `\qw`/`\qqw` handling to generalize)
- `src/parser/primary/string/escapes.rs` — `process_escape_sequence`
