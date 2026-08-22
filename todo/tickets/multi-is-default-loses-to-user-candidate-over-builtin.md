# A user `multi` candidate marked `is default` wins a tie against the builtin instead of losing to it

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/js-nutshell.rakudoc:384`).

## Root cause hypothesis

Declaring `multi prefix:<++>($a) is default { $a - 1 }` adds a user-defined multi
candidate for the built-in `prefix:<++>` operator. In real Rakudo, `++$foo` on a plain
`Int` still dispatches to the built-in numeric increment (`$foo` becomes `2`), even
though the user's candidate is also applicable and marked `is default`. mutsu instead
dispatches to the user's candidate (`$foo` becomes `0`, i.e. `$a - 1` with `$a == 1`).

`is default` in Raku only breaks ties among otherwise-ambiguous candidates that are
equally narrow; it does not make a user candidate preferred over a builtin whose own
signature is already the (or a) best match. mutsu's multi-dispatch resolution appears
to treat a user `is default` candidate as unconditionally preferred, or fails to give
the builtin operator's own (typically `Int:D`-narrowed) candidate priority over a
generic user `($a)` signature.

## Minimal repro

```raku
multi prefix:<++>($a) is default { $a - 1 }
my $foo = 1;
say ++$foo;
```

- `raku`: `2`
- `mutsu` (`target/debug/mutsu`): `0`

## Affected files (starting point)

Multi-dispatch candidate resolution / `is default` trait handling — likely in
`runtime/dispatch.rs` or wherever multi candidates are ranked (grep for `"is default"`
/ `is_default` and the prefix `++`/`--` builtin operator registration).
