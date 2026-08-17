# A role's `has %.h = (...)` default is now coerced to a Hash when composed via `does`/`but`

`role WithDefault { has %.h = (x => 1); }` followed by `my $u2 = $u but
WithDefault;` used to leave `$u2.h` as a bare `Pair` instead of a `Hash`:

```raku
role WithDefault { has %.h = (x => 1); }
my $u = 1;
my $u2 = $u but WithDefault;
say $u2.h;       # raku: {x => 1}    mutsu (before): x => 1
say $u2.h.WHAT;  # raku: (Hash)      mutsu (before): (Pair)
```

`(x => 1)` is a one-element `List` of one `Pair`, and for a `%`-sigiled
attribute that must coerce to a `Hash` (single-Pair list -> hash with that
key/value), the same way every *instance* attribute default-construction path
already does via `coerce_attr_value_by_sigil`
(`src/runtime/methods_signature.rs`). The `does`/`but`-on-a-plain-value
mixin-composition path, in the attribute-default construction loop in
`src/runtime/types/roles.rs`, built the default value straight from
`eval_decl_trait_arg` without running it through that coercion — the only
default-construction site that skipped it. Ordinary class instantiation
(`Foo.new`, or a class composing the role) was never affected, since it goes
through the instance-construction default-ctor path instead.

Fixed by coercing the evaluated default expression with
`Self::coerce_attr_value_by_sigil(raw, *sigil)` before inserting it into the
mixin map, mirroring every other default-construction site. An `@`-sigiled
default (`has @.a = (1, 2)`) was never affected — a plain `List` literal is
already an acceptable `Positional` shape without coercion — so this was
Hash-attribute-specific. Regression test: `t/role-mixin-hash-attr-default-coerce.t`.
