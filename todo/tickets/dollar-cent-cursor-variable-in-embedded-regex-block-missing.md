# `$¢` (current cursor position variable) inside a regex-embedded code block is unimplemented

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Match.rakudoc:20`).

## Repro

```raku
my $c;
'abc' ~~ /.$${ $c = $¢ }/;
say $c; # OUTPUT: «｢c｣␤»
```

- raku: `｢c｣` (a `Match` object for the single character at the current cursor position)
- mutsu (`target/debug/mutsu`): `(Any)`

## Analysis

`$¢` is a special variable available inside a regex-embedded code block (`{ ... }` inside a
regex/token/rule), representing a `Match` for the character at the cursor's *current* match
position (distinct from `$/`, the whole match-so-far, and from `$0`/named captures). mutsu does
not implement this variable at all — reading it returns the undefined default (`(Any)`) instead
of resolving to a synthesized single-character `Match`.

## Affected files (starting point)

- `src/runtime/regex.rs` / `src/runtime/regex_parse.rs` — regex-embedded code-block variable
  environment setup (where `$/`, `$0`, named captures etc. are bound for use inside `{ ... }`
  blocks embedded in a regex). `$¢` needs to be added there, bound to a `Match` representing the
  single character at the current scan position.
- Compare with how `$/` is already threaded into an embedded code block's environment — `$¢`
  needs the same mechanism, with a position-only Match value.

## Suggested next step

Grep for where `$/` gets bound in the embedded-block execution path and add a sibling binding for
`$¢` using the cursor's current byte/char offset.
