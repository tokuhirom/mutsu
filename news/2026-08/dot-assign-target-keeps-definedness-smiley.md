# `my Type:D $var .= new` no longer bakes the definedness smiley into the invocant

`my Billboard:D $billboard .= new: :advertisement("hi")` died with
`No such method 'new' on Billboard:D`. The declaration's `type_constraint` is stored
as the whole string `"Billboard:D"`, and the `.=` desugaring in
`src/parser/stmt/decl/my_decl_assign.rs` built the implicit invocant straight from it,
so the call became a bareword lookup for a package literally named `Billboard:D`.

The scalar arm now strips the trailing `:D`/`:U`/`:_` smiley (through the existing
`strip_type_smiley_suffix` helper in `parser/stmt/decl/mod.rs`, promoted to
`pub(super)`) before using the constraint as a type name. The smiley constrains the
*variable*, not the package `.new` should resolve.

The ticket also suspected the `@`/`%` arms just above of the same bug, since they build
`Array[c]` / `Hash[c]` from the same string. They are correct as they stand and were
deliberately left alone: there the constraint is the *element* type, and
`my Int:D @x .= new` really does produce `Array[Int:D]` in Rakudo — verified against
`raku`, and now pinned in mutsu too.

Found via `Type/Metamodel/Mixins.rakudoc:18,63`; fixed as a prerequisite of the
`Metamodel::Documenting` work, whose documented idiom is
`my Pod::Block::Declarator:D $pod .= new`
(`news/2026-08/metamodel-how-set-why-after-compose-immutable.md`). Pinned by
`t/metamodel-introspection.t`.
