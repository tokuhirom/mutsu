# `::?ROLE:D` / `::?CLASS:D` constrain the following parameter, not an invocant

```raku
role Foo {
    method create(::?ROLE:D :from(:$for)!) { say $for }
}
```

died with `Invalid typename 'from' in parameter declaration.` before the role body
finished. The same signature on a class (`::?CLASS:D`) did not die — it silently bound
nothing, so `Foo.create(from => 1)` saw `(Any)`.

This is what blocks `use LibXML;` (`zef:dwarring`): `LibXML::_Configurable` declares
`multi method create(::?ROLE:D :from(:$for)! is raw, |c)`.

## Root cause — a mis-read of what `::?ROLE:D` is

`::?ROLE:D` here is **not** an invocant. An invocant needs its own trailing colon
(`method m(::?CLASS:D: $x)`); without one, the type constrains the parameter that
follows. Verified against raku: `method create(Foo:D :from(:$for)!)` type-checks the
NAMED parameter (`Foo.create(from => 3)` reports
`Type check failed in binding to parameter '$for'; expected Foo but got Int (3)`) and is
callable on the type object.

The pseudo-type arm of `parse_param_inner` (`src/parser/stmt/sub_param/param_inner.rs`)
consumed `::?ROLE`/`::?CLASS` plus any definedness smiley, then decided between
"constraint on the next parameter" and "anonymous invocant" by asking whether a **sigil**
(`$ @ % & \`) followed. A named parameter starts with `:` instead, so
`::?ROLE:D :from(:$for)!` took the invocant branch — and the leftover `:from(:$for)!` was
then re-parsed from scratch as a positional sub-signature destructuring whose type name
was `from`. On the role path that name reached the role-method parameter-type validation
and threw; on the class path nothing validated it, so the method was registered with a
parameter that could never bind.

## Fix

The pseudo-type arm now also accepts a named-parameter marker as "the constraint applies
to the following parameter". The marker only counts when an identifier or sigil follows
the `:` immediately, so `::?CLASS:D : $x` keeps its invocant reading, and the pre-existing
attached-colon invocant form (`::?CLASS:D:`) is unaffected — it is matched before the
whitespace skip.

`t/role-composition-gaps.t` pins both the role declaration and, on the class side, that
`from` and its `for` alias each bind.
