# A nested type name declared inside a package is qualified by it

`module M { class A::B { } }` declares **`M::A::B`**. mutsu agreed about the
*name* — `.^name` reported `M::A::B` — but registered the ClassDef under the bare
`A::B`, so the type object and the registry disagreed and construction died:

```raku
module M { class A::B { method f { 42 } } }
M::A::B.^name     # M::A::B
M::A::B.new       # X::Method::NotFound: Unknown method value dispatch
                  # (fallback disabled): new on M::A::B
```

The same mismatch also leaked the type into `GLOBAL`: plain `A::B` resolved at
file scope, where raku reports `Could not find symbol '&B' in 'GLOBAL::A'`.

`exec_register_class_op` skipped the package qualification whenever the declared
name already contained `::`, which is exactly the nested case. It now qualifies
those like any other declaration; a name that is already the current package, or
already prefixed with it, is left alone, and `class GLOBAL::Foo` still declares
into the global namespace.

Two lookups carried the same `contains("::")` guard and had to follow, or the
qualified declaration became unreachable from inside its own package:

- `resolve_type_in_current_package` — a bareword `X::Decode` inside `module M`
  now probes `M::X::Decode` up the package chain before falling back, which is
  also what makes a package-local nested declaration shadow a same-named outer
  one;
- `qualify_sibling_parent_name` — an inheritance parent named `X::Decode`
  resolves to the sibling `M::X::Decode`, which the third link of
  `class X {}; class X::Decode is X {}; class X::Decode::Length is X::Decode {}`
  needs.

## Where this stops short of Rakudo

Rakudo does not simply install the class under its qualified name. For
`unit module M; class X::Imported::Boom is Exception { }` it records
`M::X::Imported::Boom` as the `.^name` but installs the type into the
*already-existing* outer `X::` package, so a consumer of `M` reaches it as plain
`X::Imported::Boom` and **not** as `M::X::Imported::Boom` — the shape Zef uses
for `X::Zef::UnsatisfiableDependency`. Rather than model that installation rule,
the written name is registered as an alias for the qualified declaration (never
over an existing entry), so both spellings resolve. That is a superset of
Rakudo's visibility, not a match for it.

This is not grammar-specific — a `class`, a `grammar`, or any other package
declaration with a nested name was affected equally. It came out of the YAMLish
battery work, where `Single.make-value` does `$schema.new.parse($!value)` with
`$schema` being `YAMLish::Schema::Core`, a grammar declared as
`grammar Schema::Core is Schema::JSON` inside `unit module YAMLish`.

## Follow-up: package-aware redeclaration checks

The declaration-time checks now use the same package-qualified key as type
registration, so two packages may declare the same written nested name:

```raku
module A1 { class N::C { } }
module A2 { class N::C { } }
```

These declare distinct `A1::N::C` and `A2::N::C` types. Redeclaring `N::C`
twice within one package remains an `X::Redeclaration`.

Pin: `t/nested-type-name-in-package.t` — also passes under raku.
