# The `\q[...]` / `\qq[...]` embedded-quote escapes reach every interpolation walk

`Language/quoting.rakudoc` ("Escaping") makes `\q[...]`, `\qq[...]`,
`\q:adverbs[...]`, `\qw[...]` and `\qqw[...]` legal inside every quoting
construct — the escape re-quotes its body under the named rules and splices the
result in:

```raku
say "a\qq[1+1]b";                              # a1+1b
say qq!a\qq[1+1]b!;                            # a1+1b
my $s = 'ab'; $s ~~ s/a/x\qq[1+1]y/; say $s;   # x1+1yb
```

All three died in mutsu with `Unrecognized backslash sequence: '\q'`. The gap
became newly visible in substitutions when the `s///` replacement was collapsed
onto the shared `qq` walk
(`news/2026-08/subst-replacement-is-a-qq-quote.md`), which is when the ticket
was filed.

## What was actually missing

The ticket assumed the escape was unimplemented everywhere and that the real
unit of work was collapsing the three interpolation walks in
`src/parser/primary/` onto one. Reading the code showed something better: a
**complete, general implementation already existed** —
`quote_adverbs.rs::process_q_escape` handles the whole family, `\q`, `\qq`,
`\q:adverbs`, `\qw`, `\qqw` — but it was private and reachable only from the
`q`-mode branch of `process_content_with_flags`. Every other walk had either
nothing (`double_quoted_string`, `interpolate_string_content_with_modes`, and
`process_content_with_flags`'s own `:b`/qq branch) or a partial hand-rolled
copy (`parse_single_quote_qq`, which knew `\qq` and `\qw` but not `\q`).

So the fix is not a fourth bespoke walk and does not need the full collapse:
`process_q_escape` is now the single shared entry point, called from every walk
before the character-level escape handler (it produces an expression, not a
character, so it has to run first). `parse_single_quote_qq`'s partial copy is
deleted in favour of it.

The three interpolation walks still exist and still differ in other ways; that
consolidation remains open work, but this escape family is no longer part of it
— it has exactly one implementation now.

Pinned by `t/parser-expression-gaps.t`, which exercises the escape in `"..."`,
`qq//`, `q[]`, an `s///` replacement and a heredoc.
