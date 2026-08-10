# Role BUILD/TWEAK submethods can now write array/hash attributes on a does/but-mixed value

`$value does Role` / `$value but Role`, applied to a plain (non-`Instance`)
value like an `Int` or `Str`, runs the role's `BUILD`/`TWEAK` submethods
through `run_role_submethod` (`src/runtime/types/roles.rs`). That helper
seeds the submethod's private-attribute env vars from the mixin map before
running the body, then reads modified values back afterward — but it always
used the scalar twigil form (`"!attr"`) for the seed/readback env key,
regardless of the attribute's declared sigil.

For a scalar attribute (`has $.x`) this matched what the compiled body
resolves `$!x` to. But an array or hash attribute (`has @.a` / `has %.h`)
resolves `@!a` / `%!h` through the *sigil-prefixed* env key (`"@!a"` /
`"%!h"`), which was never seeded — so any `@!a.push(...)` or `%!h<k> = v`
inside a role's `BUILD`/`TWEAK` submethod silently no-opped when the role
was composed onto a plain value via `does`/`but`.

```raku
role RH { has %.h; submethod BUILD { %!h<a> = 1 } }
my $v = 0;
$v does RH;
say $v.h.raku;   # was: "{}".Seq (empty) — now: "{:a(1)}", matching raku
```

Fixed by picking the seed/readback env key from the attribute's declared
sigil (a new `attr_env_key` helper), mirroring how ordinary instance
attributes are keyed elsewhere in the VM.

While writing the regression test, a related-but-separate bug surfaced:
`apply_role_mixin`'s attribute-default construction does not coerce a hash
attribute's default expression by sigil (`has %.h = (x => 1)` composes as a
bare `Pair`, not a `Hash`, on a does/but-mixed value) — every other
default-construction site in the codebase already does this coercion via
`coerce_attr_value_by_sigil`. Filed as
`todo/tickets/role-mixin-hash-attr-default-not-coerced.md`; it does not
affect this fix (the new test builds its hash/array attributes via
BUILD/TWEAK writes, not default expressions).

Regression test: `t/role-submethod-array-hash-attr.t`.
