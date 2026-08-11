# Calling an instance attribute accessor on a bare type object gives the wrong error

## Repro

```
class Foo { has $.x; }
say Foo.x;
```

raku:

```
Cannot look up attributes in a Foo type object. Did you forget a '.new'?
  in method x at -e line 1
  in block <unit> at -e line 1
```

mutsu (`target/debug/mutsu`):

```
No such method 'x' for invocant of type 'Foo'
  in block <unit> at -e line 1
```

## Root cause

`Interpreter::should_bypass_native_fastpath`'s `Package` (type-object) receiver
branch deliberately never calls `has_public_accessor` — only
`has_user_method`/`has_class_level_attr` — because an instance attribute's
accessor is meaningless called on the bare type (confirmed and pinned by
`resolve_user_method_or_accessor_would_wrongly_answer_for_a_package_receiver`,
`src/runtime/methods_native_bypass.rs`, added in ADR-0019 E4b step 11). So for
an accessor-only class with no other method of the same name, the call never
routes to the accessor method at all — it falls through to the native
`native_method_{0,1,2}arg` cascade, which has no arm for an arbitrary
attribute name, so `call_method_with_values` ultimately reports "no such
method" instead of dispatching to (and running) the accessor.

raku instead resolves `.x` to the real accessor `method x { ... }` even on
the type object, and the accessor's own body is what raises "Cannot look up
attributes..." when it tries to read the (nonexistent, since there's no
instance) attribute storage. mutsu already has this exact error message
implemented, but only as a narrow special case for `.name` specifically
(`methods_instance_ops.rs:1893-1907` — `.name` collides with the type-name
introspection method, so it special-cases "does this class have a `name`
accessor" before falling back to plain type-name stringification). No general
mechanism raises it for an arbitrary attribute name.

## Why this is not a quick fix here

Fixing it properly needs two things done together:
1. Route Package-receiver accessor calls to the accessor method (i.e., widen
   `should_bypass_native_fastpath`'s Package branch to also check
   `has_public_accessor`) — but naively doing this via
   `resolve_user_method_or_accessor` (as the Instance branch already does)
   changes bypass for every accessor-only Package call, not just the ones
   that should error; it needs to route into a body that does the right
   thing, not just bypass more.
2. Make the *generic* instance-attribute-read path recognize "receiver is a
   type object, not an instance" and raise the same
   "Cannot look up attributes in a {class} type object..." error the `.name`
   special case already has a copy of, instead of whatever undefined-read
   behavior currently happens when an accessor method body executes against
   a `Package` "self".

This is a real Raku-compatibility bug (wrong error type/message), not an
architectural one, so it's independent of ADR-0019's dispatch-resolver work —
filed separately rather than folded into that campaign.

## Affected files

- `src/runtime/methods_native_bypass.rs` (`should_bypass_native_fastpath`,
  Package branch)
- `src/runtime/methods_instance_ops.rs` (`.name`'s existing narrow instance
  of the correct error, for reference)
- Generic accessor-method execution path (wherever an auto-generated `has
  $.x` accessor's body actually reads instance state — not yet traced)
