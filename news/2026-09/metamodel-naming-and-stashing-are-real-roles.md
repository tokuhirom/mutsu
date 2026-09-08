# `Metamodel::Naming` and `Metamodel::Stashing` are composable roles

`Type/Metamodel/Stashing.rakudoc:45`'s worked example — the documented way to
write a minimal custom HOW — died on its first line:

```raku
class WithStashHOW
    does Metamodel::Naming
    does Metamodel::Stashing
{ ... }
```

```
X::InvalidType: Invalid typename 'Metamodel::Naming'
```

The `does` clause failed before the class body was read, because neither name
existed as anything a user could compose. This is the headline of
[#7551](https://github.com/tokuhirom/mutsu/issues/7551), which by then had had
three earlier framings closed under it, each measured wrong or already fixed
before the next was written.

## What was actually missing

Measured first, on `ac24142`, by replacing the two `Metamodel::` roles with
locally-declared stand-ins. Everything else in the example already worked: the
`WithStashHOW:_:` invocant smiley, the `:=` bindings, the `Str:D :$name!`
named parameter, `Metamodel::Primitives.create_type`, role composition, and the
`$meta.set_name(...)` dispatch. Exactly two things were absent:

1. the two role names, and
2. any path from a `create_type` type object to a name given to it afterwards —
   `create_type` mints the type with `Symbol::intern("")` and `.^set_name`'s
   handler had no `CustomType` arm, so `$type.^name` stayed `CustomType` and
   `$type.WHO` stayed the empty stash.

## The name is state on the metaobject, not on the type

The obvious implementation — record the name against the type, keyed by its id —
is wrong, and `raku` says so directly. After the example above:

```
WithStash.^name                     # WithStash
WithStashHOW.new.name(WithStash)    # ''      <- a *different* metaobject
```

Rakudo's `Metamodel::Naming` is `role { has $!name; method name($obj) { $!name } }`:
the name is an attribute of the **metaobject**, and `$obj` is ignored. `.^name`
is `$type.HOW.name($type)`, so a type minted by `create_type` reports whatever
its own `$meta` was told, and a second `WithStashHOW.new` — which was told
nothing — reports the empty string. A type-keyed store would make those two
agree, which is a different language.

So the roles are provided as real Raku source in a prelude
(`METAMODEL_ROLE_PRELUDE`, injected the way `RATIONAL_ROLE_PRELUDE` and the
`trait_mod:<does>` candidates already are), with no native primitives at all:

```raku
role GLOBAL::Metamodel::Naming {
    has $!name;
    method name(Mu $obj) { $!name // '' }
    method set_name(Mu $obj, $new_name) { $!name = $new_name }
}
role GLOBAL::Metamodel::Stashing {
    method add_stash(Mu $type_obj) { $type_obj.WHO; $type_obj }
}
```

`add_stash` "creates and sets a stash for a type, returning `$type_obj`".
mutsu's package stashes are created on demand and keyed by the type's name, so
asking for `.WHO` *is* the creation step — but the method still has to answer the
type object, because the documented HOW ends `new_type` with it.

The other half is on the Rust side: `.^name`, `.WHO` and `.WHAT` on a
`ValueView::CustomType` now ask the type's own metaobject
(`Interpreter::custom_type_how_name`), which is Rakudo's protocol taken
literally rather than a new store. A HOW that composes no naming role has no
`name` method to ask, and the type keeps the name it was minted with — so
nothing that does not opt in changes. `.^set_name` on such a type object is
routed to the metaobject's `set_name` for the same reason.

One fidelity detail worth keeping: an un-named `create_type` type object reports
the **empty string**, not a placeholder, because that is what its metaobject
answers. `None` from `custom_type_how_name` means only "this HOW has no name to
give", which is a different thing from "the name is empty".

## Verification

`t/metamodel-naming-stashing.t`, 14 assertions, passing under `raku` unmodified.
Beyond the doc's example it pins the metaobject-scoping (`WithStashHOW.new` knows
no name), the empty-string answer for an un-named type, the `set_name`/`name`
round trip, `add_stash`'s return value, `Metamodel::Naming` composing on its own,
and that a builtin type is untouched.

## Not done

The issue's two smaller residues stay deferred on their own corpus evidence: the
installed method reporting the name it was *added* under rather than the
routine's own, and a plain `sub` used as a method not receiving the invocant as
its first positional. Neither is touched here.

Also unchanged: `Metamodel::Primitives.create_type` still registers a plain
empty class and answers a bare type object, so a type minted through
`Metamodel::ParametricRoleHOW.new_type(...)` still introspects as
`ClassHOW`-shaped. That is the issue's other half and has its own repro.
