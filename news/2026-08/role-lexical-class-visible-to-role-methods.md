# A `my class` declared in a role body is visible to the role's methods

A lexically-scoped class declared inside a `role` could not be named from the
role's own methods once the role was composed. The same declaration already
worked inside a bare block and inside a `class`.

```raku
class Foo { method who() { "outer-Foo" } }
role InRole {
    my class Foo { method who() { "inner-role-Foo" } }
    method make() { Foo.new }
}
class UsesRole does InRole { }

say UsesRole.new.make.who;   # was: outer-Foo    now: inner-role-Foo
```

Worse, the name silently resolved to whatever same-named type happened to be in
the ambient env — an outer class, or another package's lexical class — instead
of raising anything.

## Root cause

`exec_register_class_op` gives a class-nested type its short-name treatment only
when the enclosing package is a *class*:

```rust
let parent_is_class = qualified_name.rsplit_once("::")
    .map(|(parent, _)| self.has_class(parent)).unwrap_or(false);
```

A role lives in `registry.roles`, not `registry.classes`, so `role R { my class
Foo {} }` registered `R::Foo` and then fell into the *module* branch, which only
adds a non-clobbering `entry_or_insert_with` alias for the short name. Nothing
recorded `Foo` as a package-scoped short name, so `resolve_suppressed_type` — the
probe that makes a class's nested types win bareword resolution inside its own
methods — could never fire for it. Bareword resolution fell through to the plain
env lookup, which is why an outer `Foo` (or an unrelated package's `Foo`) won.

Making the probe fire is only half the fix: `resolve_suppressed_type` walks
`current_package`, the `method_class_stack` and `constructing_class`, and a
composed role method runs with the *consuming class* on that stack — never the
role. Sweeping every role the class composes would let an unrelated role lend its
lexical types to the class's own methods, so instead the probe uses `::?ROLE`,
which method dispatch already binds to the originating role of the method being
run (and clears for a method that came from no role).

## Changes

- `src/vm/vm_typedecl_ops.rs`: a lexical type whose enclosing package is a role
  is recorded with `register_class_scoped_short_name`, which is what gates the
  owner-package probe. It is deliberately *not* `suppress_name`d — a role body is
  not a package boundary that hides an outer same-named type from the rest of the
  file.
- `src/runtime/runtime_encoding.rs`: `resolve_suppressed_type` probes
  `<::?ROLE>::<name>` after the class probes, so a nested type of the class
  itself still wins over one lent by a role.

## Impact

This is the shape `Cro::HTTP::Middleware` is built on: both
`Cro::HTTP::Middleware::Conditional` and `Cro::HTTP::Middleware::RequestResponse`
declare `my class Request` / `my class Response` in the role body and hand them
out from `method request()` / `method response()`. Under the old behaviour those
resolved to the top-level `Cro::HTTP::Request` / `Cro::HTTP::Response` **type
objects**, so `before-matched { … }` pushed a type object into `RouteSet`'s
`@!before-matched` and the first thing to touch it died with
`No such method 'transformer' for invocant of type 'Cro::HTTP::Request'`. They now
resolve correctly:

```
$mw.request.^name    # Cro::HTTP::Middleware::Conditional::Request
$mw.response.^name   # Cro::HTTP::Middleware::Conditional::Response
```

Pinned by `t/role-lexical-class.t`.

A neighbouring bug surfaced while writing that test and is filed separately as
`todo/tickets/class-nested-my-class-clobbers-outer-short-name.md`: a class-body
`my class Foo` permanently clobbers a file-scope `Foo`, because a class body is
not an env scope and nothing restores the outer binding.
