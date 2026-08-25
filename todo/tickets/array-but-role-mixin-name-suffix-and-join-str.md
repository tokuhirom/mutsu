# An `Array but role {...}` value loses its `+{...}` name suffix, and `join` ignores the mixin's `Str`

Originally filed as "`but`-mixing a role onto an `Array` silently drops the mixin entirely"
(doc-diff harness, `Language/perl-func.rakudoc:2281`). **Re-measured on `main` @ `17139dd55`
(2026-08-25): the mixin is no longer dropped** — the role's methods are attached and reachable,
and direct stringification uses the mixin's `Str`. Two narrower symptoms survive, so the ticket
is rescoped.

## Repro

```raku
my @origlist = (3, 2, 1);
my $r = @origlist but role { method Str { self.join("<") } };
say $r.^name;         # raku: Array+{<anon|1>}   mutsu: Array          -- BROKEN
print join(">", $r);  # raku: 3<2<1              mutsu: 3 2 1          -- BROKEN
print $r;             # raku: 3<2<1              mutsu: 3<2<1          -- matches
```

## What is actually wrong now

1. **`.^name` drops the `+{...}` composition suffix for an `Array`/`List` base.** The mixin is
   attached (the role's methods dispatch), but the type object's rendered name does not show
   it. This is base-type-specific, not a general mixin-naming gap — a class instance is already
   correct:

   ```
   $ mutsu -e 'class C {}; role R {}; my $o = C.new but R; say $o.^name;'   # C+{R}  -- matches raku
   ```

   So whatever produces the `+{...}` suffix for an `Instance` base is not reached for an
   `Array` base.

2. **`join` stringifies the mixed value with the default list stringification instead of the
   mixin's `Str`.** `print $r` correctly calls the mixin's `Str` (giving `3<2<1`), but
   `join(">", $r)` gives `3 2 1` — space-separated, i.e. the *default* list stringification —
   proving `join`'s single-argument coercion path takes a route that bypasses mixin method
   dispatch, unlike `print`'s. (Note the `>` separator never appears in `raku`'s output either:
   `join` receives one argument, so there is nothing to separate; the whole output comes from
   stringifying that one value. The bug is purely *which* `Str` is used.)

## Relationship to sibling tickets

Symptom 2 is the same family as
[list-but-role-loses-positional-binding.md](list-but-role-loses-positional-binding.md),
[hash-default-role-mixin-dropped.md](hash-default-role-mixin-dropped.md),
[role-mixed-hash-sort-method-dispatch-broken.md](role-mixed-hash-sort-method-dispatch-broken.md),
and [role-mixed-value-gist-skipped-in-array.md](role-mixed-value-gist-skipped-in-array.md) —
a role-mixed value's method dispatch not surviving a *particular* generic coercion or storage
path, while other paths handle it. Since `print` now works and `join` does not, comparing those
two coercion paths is the most direct way in.

## Affected files (starting point)

- `src/runtime/mixin.rs` and the `.^name` rendering in `src/value/types.rs`
  (`role_mixin_suffix_excluding` and callers) for symptom 1.
- `join`'s argument-stringification path for symptom 2 — diff it against `print`'s, which
  already dispatches the mixin's `Str` correctly.
