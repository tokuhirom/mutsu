# `$:name` named-placeholder variable fails to interpolate inside a double-quoted string

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Type/Code.rakudoc:81`).

## Root cause hypothesis

`$:name` is the named-placeholder-parameter form (like `$^a`/`$^b` for positional
placeholders, used inside a sub body to implicitly declare a named parameter). It
works fine as a bare statement/expression, but when interpolated inside a
double-quoted string, mutsu's string-interpolation scanner does not recognize
`$:name` as a variable at all — it apparently treats the bare `$` (immediately
followed by `:`) as a literal, unescaped `$` sigil, which the general "a `$` must
either start a variable or be backslashed" check then rejects.

## Minimal repro

```raku
sub foo { say $:foo }
&foo.assuming(foo => 42)();          # OK on mutsu: prints 42

sub bar { say "$:foo" }
&bar.assuming(foo => 42)();          # mutsu errors; should print 42
```

- `raku`: both print `42`.
- `mutsu` (`target/debug/mutsu`): the bare-variable form works (`42`), but the
  interpolated form throws:
  ```
  Runtime error: Non-variable $ must be backslashed
  ```

The doc's own combined example (`"$^a $^b $:foo $:bar"`) fails the same way for the
same reason — `$^a`/`$^b` positional-placeholder interpolation already works, only the
`$:name` named form is affected.

## Affected files (starting point)

- The double-quoted-string interpolation scanner (parser/quoting code that decides
  what follows a `$` inside `"..."` — grep for where `$^` positional-placeholder
  interpolation is already handled, since that sibling form works; the `$:name` case
  needs the analogous branch).
