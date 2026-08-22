# `but`-mixing a role onto an `Array` silently drops the mixin entirely

Found by the doc-diff harness batch-4 re-run (`docs/doc-diff-backlog.md`,
`Language/perl-func.rakudoc:2281`).

## Repro

```raku
my @origlist = (3, 2, 1);
my $r = @origlist but role { method Str { self.join("<") } };
say $r.^name;
print join(">", $r);
```

- `raku`: `Array+{<anon|1>}`, then `3<2<1` (`join(">", $r)` treats the mixed value's
  single-argument list as a plain three-item list, joining with `>`: `3>2>1` — the
  `Str` override on the mixin isn't what produces the `>`-separated output, plain
  positional-list `join` semantics already do; the doc's own example is really about
  `.Str` mattering when the value is stringified directly, e.g. by `print`).
- `mutsu` (`target/debug/mutsu`): `Array` (the mixin is invisible on `.^name` — the
  role composition did not take effect at all), then `3 2 1` (space-joined, i.e. `join`
  received the mixed value as a single flattened-as-list argument using the *default*
  list stringification, confirming the mixin's `Str` override was never attached to the
  value either).

## Root cause hypothesis

mutsu's `but`-mixin path for an `Array`/`List` value appears to no-op silently instead
of producing a role-composed value — `.^name` shows the plain `Array` type with no
`+{...}` suffix, and neither the new `Str` method nor (going by `join`'s output) any
role metadata survived. This is almost certainly the same family of bugs already
tracked by
[list-but-role-loses-positional-binding.md](list-but-role-loses-positional-binding.md),
[hash-default-role-mixin-dropped.md](hash-default-role-mixin-dropped.md), and
[role-mixed-value-gist-skipped-in-array.md](role-mixed-value-gist-skipped-in-array.md)
— all involve a `but`/`does`-mixed value's role metadata not surviving a generic
storage/dispatch path. This finding's minimal repro is the *plainest* case so far (no
binding, no Hash default, no array nesting — a bare `Array but role {...}` loses the
mixin outright), so it may be the best starting point for isolating the actual fix
site.

## Affected files (starting point)

- `src/runtime/mixin.rs` (or wherever `but`/`does` role-mixing on an `Array`/`List`
  value is implemented) — check what `@origlist but role {...}` actually produces
  (`--dump-ast` plus a direct `.^name` check) to see whether the mixin call itself is a
  no-op for Array-typed operands, or whether the composed value is created correctly
  but lost/unwrapped somewhere between the `but` expression and the variable binding.
