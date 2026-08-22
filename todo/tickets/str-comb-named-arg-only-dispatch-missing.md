# `Str.comb(:match)` (named-arg-only call, no positional matcher) fails to dispatch at all

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Str.rakudoc:647`).

## Root cause

Plain `.comb` (no args) works. But `.comb(:match)` — called with only a named argument and
no positional matcher/limit — fails to dispatch entirely, as if no `comb` method exists on
`Str` at all:

```raku
say "abc".comb.raku;          # raku AND mutsu: ("a", "b", "c").Seq   -- matches
say "abc".comb(:match).raku;  # raku: (｢a｣ ｢b｣ ｢c｣)
                               # mutsu: No such method 'comb' for invocant of type 'Str'
```

This looks like the native-method arity dispatch (`native_method_*arg`, see
`builtins/methods_0arg/` vs `methods_narg.rs`) has no registered arm for "0 positional
args + 1 named arg", so it falls through past `comb`'s normal implementations to nothing
at all (not even a slow-path fallback) rather than being caught by the argument-count
matching that presumably handles the other forms (`.comb(3)`, `.comb(3, 2)`,
`comb(/\w/, ...)` etc., which the same doc block confirms already work correctly).

## Minimal repro

```raku
say "abc".comb(:match).raku;
```

- `raku`: `(｢a｣ ｢b｣ ｢c｣)`
- `mutsu` (`target/debug/mutsu`): `No such method 'comb' for invocant of type 'Str'`

## Affected files (starting point)

`Str.comb` method dispatch — wherever `comb` arity variants are registered (likely
`builtins/methods_narg.rs` / `methods_0arg/`). Needs a 0-positional+named-`:match` arm
that returns `Match` objects instead of plain `Str`s for each matched grapheme/substring.
