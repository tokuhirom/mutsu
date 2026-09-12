# Two false `Invalid typename ... in parameter declaration` rejections in role bodies

The ecosystem parity ledger grouped 21 distributions under one failure line,
`Invalid typename '<X>' in parameter declaration.`
([#7993](https://github.com/tokuhirom/mutsu/issues/7993)), across 14 different
typenames. The ticket's own guess was that the names pointed at several
unrelated resolution paths and the first job was to find out how many gaps they
really were. Minimising one case from each group found **four** distinct root
causes hiding behind the single message. Two of them are fixed here.

## 1. An exported `subset` declared in a role body was invisible until composition

`PDF::COS` hands its types out from inside a role body:

```raku
role PDF::COS {
    my subset IndRef of Pair is export(:IndRef) where {.key eq 'ind-ref'};
    ...
}
```

and `PDF::COS::Tie` imports it and names it in a signature:

```raku
role PDF::COS::Tie {
    use PDF::COS :IndRef;
    multi method deref(IndRef $ind-ref!) { ... }
}
```

mutsu defers a role body wholesale to *composition* — correct for the body's
runtime statements (rakudo runs a role body once per composition, not at the
declaration), but wrong for a **declaration**. Rakudo installs a `my subset` in
the role body's pad, and its `is export` entry in the module's export table, when
the role is compiled. Deferring it meant `use PDF::COS :IndRef` imported nothing:
the type was not registered, `IndRef` decayed to a bareword `Str`, and every type
check against it silently failed.

That was the quiet half. The loud half was the ledger's message. `PDF::COS::Tie`'s
signature is validated when a class composes the role, in a scope where `IndRef`
is not lexically visible. The validator has an escape hatch for exactly this —
"a module this body `use`s has not been loaded yet, so defer" — but by the time
`PDF::COS::Array` composes the role, `PDF::COS` *is* loaded (it is `use`d two
lines earlier), the hatch no longer applies, and the unregistered `IndRef` was
reported as a bogus typename.

`register_role_body_exported_subsets` now registers an `is export`ed `subset`
declared directly in a role body at role-declaration time as well as at
composition. Role-private subsets are left alone: they are already accepted in
their own body via `RoleDeclCx::body_declared_types` and have no cross-module
consumer to serve. A subset whose base type is one of the role's own type
parameters (`role R[::T] { my subset S of T … }`) is skipped too — its base is
not known until the role is parameterised, so composition stays the only correct
point for it.

Six PDF-family distributions (`PDF`, `PDF::Class`, `PDF::Content`,
`PDF::Font::Loader`, `FDF`, `Pod::To::PDF::Lite`) are this shape — a third of
the cluster. They now get past the typename error to their next, unrelated
blocker. (`LibXML`'s `NCName` failure looks similar but is not this: its
`LibXML::Types` declares the subsets at module scope, and the failure does not
reproduce outside the sweep's dependency closure. It needs a re-measure.)

## 2. A role method parameter naming a sibling type relatively

`SQL::Abstract` is a `unit class`, so its nested `class Column::List` registers
as `SQL::Abstract::Column::List` — but every signature in the file spells it with
the relative `Column::List`:

```raku
unit class SQL::Abstract;
class Column::List does Value::List { ... }
role Distinction {
    multi method COERCE(Column::List(Any) $columns) { ... }
}
```

The role-method validator already walks the enclosing packages to resolve a
sibling type, but it did so only for a constraint with no `::` in it, and it
prefixed the *raw* constraint rather than the undecorated base name. So a
relatively-qualified name never reached the walk at all, and a decorated one
(`Column::List(Any)`, `Foo::Bar:D`) could not have matched even if it had. The
walk now uses the stripped base name and is no longer gated on the constraint
being unqualified. A genuinely undeclared qualified name still reports
`X::Parameter::InvalidType`.

## The two gaps this did not fix

Recorded as their own tickets, since they turned out to be unrelated machinery:

- **A `my`-scoped declaration with a compound name does not install into the
  named package.** `TAP.rakumod` declares `role Entry` and then
  `my role Entry::Handler`; rakudo installs `Handler` into the `Entry` package,
  so `TAP::Entry::Handler` resolves. mutsu does not, and rejects the later
  `my class State does TAP::Entry::Handler`. Affects `TAP`, `App::Mi6`,
  `Mi6::Helper`.
- **`class Foo does SomeRole` where the role's nested class inherits `Attribute`.**
  With gap 1 fixed, the PDF family's next error is
  `'PDF::COS::Tie::COSAttr::CosOfAttr' cannot inherit from 'Attribute' because it
  is unknown` — a missing MOP type, nothing to do with typename resolution.

The remaining names in the cluster (`GType`, `Sizing`, `EncodeBuffer`,
`NcplaneHandle`, `Enumeration`, `Event:D`, `License::Software::Year`) sit behind
distribution dependencies that are not resolvable outside the sweep, or behind
earlier parse failures; they are left on #7993 for the next pass.
