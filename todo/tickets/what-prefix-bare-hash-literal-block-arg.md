# `WHAT {...}` (bare hash-literal block, no parens/var) misparses as two statements

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Hash.rakudoc:65`).

## Root cause

`WHAT`/`HOW`/`WHO` etc. are prefix pseudo-routines that take a term. When that term is a
bare `{...}` immediately following the prefix — with no enclosing parens and no
intermediate variable — mutsu does not recognize `{...}` as the argument term. Instead it
parses `WHAT` as a bare identifier (which stringifies to the literal string `"WHAT"`) and
`{...}` as a separate block statement run in sink context (hence the accompanying
"Useless use of ... in sink context" warning).

Every other shape works correctly:

```
say WHAT(3);              # (Int)  -- parens: OK
say WHAT 3;                # (Int)  -- bare non-block term: OK
my $h = {3=>4}; say WHAT $h;  # (Hash) -- via variable: OK
say WHAT {3 => 4};          # should be (Hash), mutsu prints "WHAT"  -- BROKEN
```

## Minimal repro

```raku
say WHAT {3 => 4};
```

- `raku`: `(Hash)`
- `mutsu` (`target/debug/mutsu`): prints `WHAT` to stdout, plus a stderr warning
  `Useless use of "=>" in expression "3 => 4" in sink context`.

## Affected files (starting point)

Likely the parser's `WHAT`/`HOW`/`WHO`/`WHY` prefix-routine handling (identifier_call /
primary parsing) — needs to special-case a following `{` as starting a hash-literal/block
term argument, the same disambiguation `say {...}` already needs for direct hash-literal
args to other listops.
