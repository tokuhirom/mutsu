# A punned role ignores its own `method new` and seeds positionals by index

Constructing a bare role (`R.new(...)`, which puns the role to a class) never
runs a `new` the role declares. `dispatch_new`'s role branch
(`src/runtime/methods_object_dispatch_new.rs`, the `registry().roles.get(...)`
arm) builds the punned instance itself: it maps each named argument onto the
attribute of that name, and — the part raku has no equivalent for — maps each
*positional* argument onto the attribute at the same index.

```raku
my role R { has $.attr; multi method new(Int:D $n) { say "in new(Int)"; self.bless(attr => "x") } }
say R.new(42).attr;
# raku:  "in new(Int)" then "x"
# mutsu: 42          (the multi never runs; 42 is seeded into $!attr positionally)
```

The class path has no such shortcut — `class C does R { }` dispatches to the
role-composed `new` correctly. Only the pun is affected.

## Why it is not a one-line fix

The role branch is a parallel construction path, not a thin wrapper over the
class one: it assembles the instance itself and mirrors each attribute into a
`__mutsu_attr__` mixin marker (the marker/cell split that used to sit under this
is resolved — see
`news/2026-07/punned-role-container-attribute-store.md`). Making it
honour a user `new` means either dispatching to the role's method table before
this branch — which has to avoid re-entering `dispatch_new` for the `self.bless`
inside that method — or routing the pun through `ensure_role_punned_to_class`
and the ordinary class constructor, which is the same consolidation that ticket
describes. The positional-by-index seeding also has to survive for roles that
declare no `new`, since existing tests rely on it.

## Where it is currently papered over

`role_attribute_types` type-checking (added with the role attribute type
constraint fix) deliberately skips positionally-seeded values: type-checking
them turns this gap into a hard `Type check failed in assignment to $!attr`
during `R.new(42)`, which `t/class-type-object-coercion-call.t` catches. The
`type_checked` flag in that loop is the marker to remove once this is fixed.
