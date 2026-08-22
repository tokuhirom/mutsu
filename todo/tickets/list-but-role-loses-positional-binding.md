# `but`-mixing a role onto a `List` breaks its `Positional` binding

Discovered via the doc-diff harness on `raku-doc/doc/Language/objects.rakudoc` (around line
1457).

## Repro

```
role R { method Str() {'hidden!'} };
my @positional := <a b> but R;
say @positional.^name;
```

- raku: `List+{R}`
- mutsu: `Type check failed in binding; expected Positional but got List`

## Root cause guess

`but`-mixing a role onto a `List` value should produce a value that both keeps its `Positional`
role (so `@`-sigil binding still type-checks) and gains the mixed-in role's methods. mutsu's
`but`-mixin path for a `List` likely wraps/replaces the value in a way that its `Positional`-ness
is no longer visible to the binding type-check, even though the underlying data is still
list-shaped.

**Possibly the same underlying root cause as** [hash-default-role-mixin-dropped.md](hash-default-role-mixin-dropped.md)
and [role-mixed-value-gist-skipped-in-array.md](role-mixed-value-gist-skipped-in-array.md) — all
three involve a `but`/`does`-mixed value's role metadata not surviving a generic storage/dispatch
path (binding type-check here, hash default-value storage there, array-element gisting there).
Filed as separate tickets since each has a distinct minimal repro and it isn't yet confirmed they
share one fix site, but worth investigating together — if a single root cause is found, merge
these into one PR.

## Affected files (starting point)

- `src/vm/vm_var_ops.rs` / wherever `:=` binding does its `Positional` type-check
- `src/runtime/mixin.rs` (or wherever `but`/`does` role-mixing on a `List`/Array value is
  implemented)

## Suggested next step

Check what `<a b> but R` actually produces (`--dump-ast` plus a direct `.^name`/`.WHAT` check
without going through the `:=` binding) to see whether the mixed value itself lost its
`Positional` role, or whether it's specifically the binding type-check that fails to recognize
it.
