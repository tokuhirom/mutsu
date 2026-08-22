# `my %h .= push(pair)` on a freshly-declared Hash should leave `%h` empty

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Hash.rakudoc:336`).

## Root cause (not fully diagnosed)

```raku
my %h .= push(e => 6);
say %h.raku; # OUTPUT: «{}␤»
```

`Hash` has no `.push` method of its own; the call resolves through some fallback (likely
`Any`/`Cool`/`List`-ish coercion), and whatever `.push` returns gets `.=`-reassigned back
into `%h`. Per raku, the net result is that `%h` ends up as an *empty* Hash (`{}`), not a
Hash containing the pushed pair.

mutsu instead resolves `.push` on `%h` as if it mutates the hash in place and keeps the
pair, giving `{:e(6)}`.

## Minimal repro

```raku
my %h .= push(e => 6);
say %h.raku;
```

- `raku`: `{}`
- `mutsu` (`target/debug/mutsu`): `{:e(6)}`

## Affected files (starting point)

Whatever handles `.push` dispatch on a `Hash` invocant when there is no native
`Hash.push` — likely falls through to a generic/slow-path push handler in
`runtime/methods.rs` or `builtins/methods_narg.rs`. Needs investigation into what raku's
`.push` resolution on `Hash` actually returns (probably it resolves against `Any.push`,
which does something unrelated to hash mutation, and the `.=` reassignment then replaces
`%h` with that unrelated return value coerced back through the Hash container type,
landing on an empty hash). Not root-caused further within this session's time budget.
