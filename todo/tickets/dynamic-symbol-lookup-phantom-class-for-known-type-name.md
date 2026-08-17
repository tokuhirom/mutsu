# Dynamic symbol lookup (`::($name)`) returns a phantom class for a "known type name" that isn't actually registered

Discovered while building ADR-0029 Slice 2's mechanical raku-vs-mutsu diff
script (`scripts/adr0029-capture-x-exception-data.py`,
`scripts/probe-x-exception-shape.raku`): dynamic symbol lookup on a compound
name (`::($name)` where `$name` contains `::`) does not distinguish "a real
registered class" from "a name the parser/type-checker layer merely
*recognizes* as a valid type constraint" (`is_known_compound_type` /
`is_known_type_constraint` in `src/runtime/utils/type_constraints.rs`, a
large hardcoded match list consulted from a dozen call sites across the
parser and runtime for signature/`given`/ternary/role-body type-name
disambiguation).

## Repro

```
$ target/debug/mutsu -e '
my $type = ::("X::Anon::Augment");
say $type.^name;             # X::Anon::Augment (looks right)
say $type.^mro.map(*.^name); # (X::Anon::Augment Any Mu)  -- no Exception!
say $type ~~ Exception;      # True                       -- inconsistent
say $type.WHAT;              # (Augment)                  -- wrong short name in gist
'
$ target/debug/mutsu -e 'X::Anon::Augment.new'
X::Method::NotFound: Unknown method value dispatch (fallback disabled): new on X::Anon::Augment
```

So `X::Anon::Augment` is genuinely unregistered (`.new` correctly fails), but
`::("X::Anon::Augment")` silently returns *something* that smartmatches
`Exception` and carries a `ClassHOW`, rather than the `Failure` real `raku`
returns for the same unregistered name. This makes any code path that
resolves a type by dynamic string and then trusts `~~`/`.HOW` (rather than
calling a method on it) get a false positive.

## Root cause

`src/runtime/accessors_stash.rs` (~line 253-259), the dynamic-symbol-lookup
helper:

```rust
if name.contains("::")
    && (crate::runtime::utils::is_known_compound_type(name)
        || self.has_class(name)
        || self.is_role(name))
{
    return Value::package(Symbol::intern(name));
}
```

`is_known_compound_type` succeeds for any name in the large hardcoded
whitelist in `type_constraints.rs` (which includes almost every `X::` name
mentioned anywhere, including ones never registered via `register_x`), and
the branch returns a *bare* `Value::package(Symbol::intern(name))` with no
check that the class actually exists in `registry.classes`. Downstream,
`.^mro` for this unregistered-but-"known" name falls through to
`classhow_mro_names`'s builtin/default branch (`vec![class_name.clone()]`
plus `Any`/`Mu`), which is why `Exception` is missing from the chain, and
some other path (not yet identified) makes `~~ Exception` answer `True`
anyway despite `Exception` not being in that mro — that inconsistency is
itself worth another look once the root symbol-resolution gap is fixed.

## Why this is a separate finding, not part of ADR-0029

`docs/adr/0029-exception-class-role-membership.md` is about *modelling* `X::`
ancestry correctly for classes that *are* registered; it doesn't touch
`::($name)`/`is_known_type_constraint`. This bug is about the gap between
"the parser accepts this bareword as a type-shaped name" and "the runtime
has actually registered a class for it" -- a different, and probably
architecturally bigger, seam (the `is_known_type_constraint` whitelist is
consulted from ~14 call sites; fixing this properly likely means making
dynamic lookup consult the *same* registry check `X::Foo.new` already uses,
not adding another special case).

## Suggested fix direction (not investigated further)

`accessors_stash.rs`'s compound-name branch should not treat
`is_known_compound_type` as sufficient on its own to return a package value
identical in shape to a real registered class -- either gate it on
`self.has_class(name) || self.is_role(name)` (dropping the
`is_known_compound_type` disjunct entirely, if nothing relies on the
loose acceptance for `::($name)` specifically), or mark the returned package
value as an unresolved/"known-but-not-a-class" stub that `~~`/`.^mro`
consumers can distinguish from a real registration.

## How it was worked around for Slice 2

`scripts/probe-x-exception-shape.raku` treats a resolved type as "not
really found" whenever `Exception` is absent from its `.^mro` list (true
for every genuinely `register_x`-registered class, since `register_x`
always appends `Exception` if missing) -- this is a correct proxy for Slice
2's purposes and does not need this ticket fixed first.
