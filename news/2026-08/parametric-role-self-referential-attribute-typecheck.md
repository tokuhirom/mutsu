# A parametric role's self-referential attribute type resolves, and `Owner::Name` stops parsing as a parameterisation

```raku
role Box[::Type] {
    has Box[Type] $.child;
    has Type      $.val;
}
my $b = Box[Int].new(val => 1);
```

died with
`Type check failed in assignment to $!child; expected Box[Int]::Box[Int] but got Box[Type] (Box[Type])`.
raku builds the instance and reports `Box[Int]`.

## Root cause — two independent bugs, both visible in that one message

**The doubled expected type.** `collect_attribute_type_constraints` re-resolves each
attribute's declared type through `resolve_type_name_for_owner(owner, tc)`, which probes
`"{owner}::{name}"` before falling back to the name itself. With `owner = "Box[Int]"` and
`tc = "Box[Int]"` (already correctly substituted at composition) the probe builds
`"Box[Int]::Box[Int]"` — and `has_type_direct` *accepted* it, because
`parse_parametric_type_name` only checked "contains `[`" and "ends with `]`". It split
that string at the first bracket into base `Box` (a real role) plus the junk argument
`Int]::Box[Int`, so the concatenation looked like a genuine parametric type and won the
probe.

**The unsubstituted default.** For an uninitialised typed scalar attribute the parser
synthesizes a default expression that is a bareword of the declared type, so the
attribute defaults to its type object (`has_decl.rs`, "Auto-default"). For
`has Box[Type] $.child` that bareword is the literal string `Box[Type]`, and nothing
substituted the role's type parameters inside the brackets — a bare `Type` resolved
through the env binding, but `Box[Type]` did not. So the attribute's default value was
the *unbound* `Box[Type]` type object, which then failed the type check against its own
concrete `Box[Int]` constraint.

## Fix

`parse_parametric_type_name` now requires the bracket that opens the parameterisation to
be closed by the final character, so a name that merely *starts* with a parameterisation
and continues afterwards is not a parametric type. `Box[Int]::Box[Int]` therefore fails
`has_type_direct`, and `resolve_type_name_for_owner` correctly returns `Box[Int]`.

`exec_get_bare_word_op` resolves a bareword naming a parameterised type through
`resolved_type_capture_name`, which already knows how to substitute bound generic type
parameters inside brackets. The gate is deliberately "the resolution differs from the
same *normalization* of the written name", not "differs from the written name": that
helper also re-joins the arguments without spaces, and comparing raw spellings retargeted
`Hash[Array, Cro::HTTP::Router::PluginKey]` to the space-free spelling, which then failed
its own attribute type check in Cro::HTTP::Router.

Note the ticket's second hypothesis — that an unset attribute should not be type-checked
at all — was wrong: raku genuinely gives `$b.child` the `Box[Int]` type object, and
checking that value against `Box[Int]` is correct once both halves resolve.

Pinned by `t/role-composition-gaps.t`.
