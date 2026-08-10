# A role's `has %.h = (...)` default is not coerced to a Hash when composed via `does`/`but`

Found while writing a regression test for
`todo/tickets/role-submethod-array-hash-attr-key-mismatch.md` (fixed in
`run_role_submethod`). This is a different, adjacent bug in
`apply_role_mixin` (`src/runtime/types/roles.rs`, the default-value
construction loop a few dozen lines above `run_role_submethod`).

## Repro

```raku
role WithDefault { has %.h = (x => 1); }
my $u = 1;
my $u2 = $u but WithDefault;
say $u2.h;       # raku: {x => 1}    mutsu: x => 1
say $u2.h.WHAT;  # raku: (Hash)      mutsu: (Pair)
```

`(x => 1)` is a one-element `List` of one `Pair`, and for a `%`-sigiled
attribute that must coerce to a `Hash` (single-Pair list → hash with that
key/value), the same way every *instance* attribute default-construction
path already does via `coerce_attr_value_by_sigil`
(`src/runtime/methods_signature.rs:36`, called from ~10 other default/ctor
sites — see `attr_build_defaults.rs`, `methods_object_dispatch_new.rs`,
etc.). `apply_role_mixin`'s default-value loop does not call it:

```rust
} else if let Some(default_arg) = default_expr {
    self.eval_decl_trait_arg(default_arg)?
} else {
    ...
};
mixins.insert(format!("__mutsu_attr__{}", attr_name), value);
```

## Why this only shows up on `does`/`but`-mixed plain values

Ordinary class instantiation (`Foo.new` or a class composing the role)
builds its attribute defaults through the instance-construction default-ctor
path, which already coerces by sigil. Only the `does`/`but`-on-a-plain-value
composition path (mixins, not instances) goes through `apply_role_mixin`'s
own default-value loop and skips the coercion.

An `@`-sigiled default (`has @.a = (1, 2)`) does NOT show this bug — a plain
`List` literal is already an acceptable `Positional` shape without
coercion — so this is Hash-attribute-specific (a bare `Pair`/list-of-`Pair`
default needs the List→Hash promotion that `coerce_attr_value_by_sigil`
performs).

## Fix sketch

Wrap the `value` computed in `apply_role_mixin`'s attribute-default loop
with `Self::coerce_attr_value_by_sigil(value, *sigil)` before inserting into
`mixins`, mirroring every other default-construction site.
