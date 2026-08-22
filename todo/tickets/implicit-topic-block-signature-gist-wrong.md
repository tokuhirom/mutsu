# `.signature` of a bare `{;}` block (implicit `$_` parameter) gists wrong

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Block.rakudoc:17`).

## Minimal repro

```raku
say {;}.signature;
```

- `raku`: `(;; $_? is raw = OUTER::<$_>)`
- `mutsu` (`target/debug/mutsu`): `($$_?)`

## Root cause hypothesis

A block with no explicit parameter list (`{;}` — the `;` forces it to parse as a Block, not
a Hash) gets an implicit `$_` parameter with these traits: optional (`?`), `is raw`, and
defaulting to the outer `$_` (`= OUTER::<$_>`), placed in the *invocant* part of the
signature (before the top-level `;;` separator, since it's an implicit self-like binding
slot) — the doc's own gist format is `(;; $_? is raw = OUTER::<$_>)`.

mutsu's `.signature` gist for this implicit-topic parameter renders as `($$_?)` — this looks
like a garbled/malformed rendering (a stray extra `$`, missing the `is raw`/`OUTER::<$_>`
default annotation, and not placed after the `;;` invocant-separator) rather than a
deliberately different but equivalent representation. Likely the signature-gisting code has
a generic path for named/positional parameters that doesn't special-case the synthetic
implicit-`$_` parameter Raku attaches to a bare block.

## Affected files (starting point)

- Wherever a block's implicit `$_` parameter is synthesized (parser/compiler for bare
  `{...}` blocks) and wherever `.signature`'s `.gist`/`.raku`/`.Str` renders a `Signature`
  object (grep for `signature` gisting in `src/runtime/` or `src/builtins/`).
