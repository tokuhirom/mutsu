# `X::Phaser::PrePost` failure message drops the source text of the failed `PRE`/`POST` condition

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/X/Phaser/PrePost.rakudoc:15`).

## Root cause hypothesis

When a `PRE { ... }` (or `POST { ... }`) phaser's condition evaluates false, Raku raises
`X::Phaser::PrePost` with a message that quotes the condition's own source text:

```raku
sub f($x) { PRE { $x ~~ Int } };
f "foo";
CATCH { default { put .^name, ': ', .Str } };
```

- `raku`: `X::Phaser::PrePost: Precondition '{ $x ~~ Int }' failed`
- `mutsu`: `X::Phaser::PrePost: Precondition '' failed`

mutsu constructs the same exception type and message shape, but the quoted condition text is
always empty. The exception-construction site for `X::Phaser::PrePost` is presumably passing an
empty/unfilled string for the condition-source field instead of the phaser body's original
source text (or a `.raku`/gist-style reconstruction of it).

## Minimal repro

```raku
sub f($x) { PRE { $x ~~ Int } };
f "foo";
CATCH { default { put .^name, ': ', .Str } };
```

- `raku`: `X::Phaser::PrePost: Precondition '{ $x ~~ Int }' failed`
- `mutsu` (`target/debug/mutsu`): `X::Phaser::PrePost: Precondition '' failed`

## Affected files (starting point)

- Wherever `PRE`/`POST` phasers are compiled and their failure raises `X::Phaser::PrePost` —
  likely in the phaser-handling compiler code (`src/compiler/`) and/or the exception
  construction site in `src/runtime/` — the phaser's original source span needs to be captured
  at compile time and threaded into the exception's condition-text field.
