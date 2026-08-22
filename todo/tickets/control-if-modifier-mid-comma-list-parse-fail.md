# Statement-modifier `if` in the middle of a parenthesized comma list fails to parse

Discovered via the doc-diff harness on `raku-doc/doc/Language/control.rakudoc` (around line
263). This is a hard parse failure, distinct from the already-deferred "`and`/`or`/`not`
word-logical precedence" cluster (that cluster is about word-operator precedence, not
statement-modifier `if`/`unless`/`for` placement).

## Repro

```
say (1, 2 if True, 3);
```

- raku: `(1 2)` — the statement-modifier `if` applies to the whole preceding comma expression
  (`1, 2`), and `, 3` starts a fresh element... actually per raku's own precedence rules the
  modifier binds loosely enough that this parses and evaluates to `(1 2)`.
- mutsu: `===SORRY!=== Confused. expected statement: expected '.' or digits ...` — a hard parse
  error, not just a wrong value.

## Root cause guess

mutsu's parser for a statement-modifier `if`/`unless`/`for`/`while` inside a parenthesized
term/comma-list doesn't expect another comma-separated element to follow the modifier's
condition, and fails to parse rather than mis-evaluating. Likely in the parser's handling of
`(...)` list parsing when a comma-list element carries a trailing statement modifier.

## Affected files (starting point)

- `src/parser/` — parenthesized list / comma-list parsing, statement-modifier attachment

## Suggested next step

Isolate whether the failure is specific to a modifier mid-list (`(1, 2 if True, 3)`) vs. a
modifier at the very end (`(1, 2 if True)`, which likely already works) to scope the parser fix
precisely.
