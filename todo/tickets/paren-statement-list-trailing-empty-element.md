# A parenthesized `;`-separated statement list drops its trailing empty-statement element

Discovered via the doc-diff harness on `raku-doc/doc/Language/list.rakudoc` (around line 54).

## Repro

```
say('foo';);
```

- raku: `(foo)()` — the list has 2 elements: `"foo"` and the empty list `()` produced by the
  trailing empty statement after the last `;`
- mutsu: `(foo)` — only 1 element

Confirmed via `--dump-ast`: mutsu compiles `('foo';)` to `ArrayLiteral([Literal(Str("foo"))])`,
silently dropping the value of the trailing empty statement.

## Root cause guess

The parenthesized statement-list-to-list compiler/parser path (`(...)` containing `;`-separated
statements, evaluated as a list of each statement's value) doesn't emit a value for a trailing
empty statement — likely because an empty statement (nothing between the last `;` and the
closing `)`) isn't represented as an AST node at all, so there's nothing to compile into a list
element.

## Affected files (starting point)

- `src/parser/` — parenthesized statement-list parsing
- `src/compiler/expr.rs` — compiling a statement-list term into a list value

## Suggested next step

Check whether the parser needs to synthesize an explicit "empty statement → empty list" node
when it sees a trailing `;` with no following statement inside `(...)`, mirroring how a bare
`;` mid-list already must produce *some* statement boundary.
