# A `my class` declared in a role body is invisible to the role's methods

A lexically-scoped class declared inside a `role` cannot be named from the
role's own methods once the role is composed. The same declaration works inside
a bare block and inside a `class`.

```raku
class Foo { method who() { "outer-Foo" } }

class InClass {
    my class Foo { method who() { "inner-class-Foo" } }
    method make() { Foo.new }
}
role InRole {
    my class Foo { method who() { "inner-role-Foo" } }
    method make() { Foo.new }
}
class UsesRole does InRole { }

say InClass.make.who;         # inner-class-Foo   (both)
say UsesRole.new.make.who;    # raku: inner-role-Foo
                              # mutsu: X::Undeclared::Symbols: Foo used at line 1
```

When the role-lexical name collides with an outer class AND the inner class
composes a role of its own, mutsu silently resolves to the **outer** class
instead of dying:

```raku
class Foo { method who() { "outer-Foo" } }
role R {
    my class Foo does Callable { method who() { "inner-Foo" } }
    method make-foo() { Foo.new }
}
class C does R { }
say C.new.make-foo.^name;     # raku: R::Foo    mutsu: Foo
```

## Why it matters

`Cro::HTTP::Middleware` is built entirely on this pattern: both
`Cro::HTTP::Middleware::Conditional` and
`Cro::HTTP::Middleware::RequestResponse` declare `my class Request` /
`my class Response` inside the role body and hand them out from
`method request()` / `method response()`. In mutsu those resolve to the
top-level `Cro::HTTP::Request` / `Cro::HTTP::Response` **type objects**, so
`before-matched { … }` pushes a `Cro::HTTP::Request` type object into
`RouteSet`'s `@!before-matched`. The first thing that touches it —
`Handler::!append-middleware`'s `$comp.transformer($current)` — then dies with
`No such method 'transformer' for invocant of type 'Cro::HTTP::Request'`, which
surfaces at the call site as the misleading
`No such private method 'append-middleware' for invocant of type
'…::DelegateHandler'`.

This is what makes `t/router-auth.rakutest` run 0 tests in each of its two
subtests, and it blocks every Cro middleware test
(`t/http-middleware.rakutest` 0/7).

## Where to look

- Role body parsing/registration: `src/parser/stmt/class/` (the role declarator)
  and `src/vm/vm_register_ops.rs`'s role registration.
- A composed method is compiled against the role's body; the role's lexical
  type registrations have to be in scope for it. Compare with how a `class`
  body's `my class` is registered — that path already works, so the fix is
  likely to make the role declarator use the same scope handling rather than
  inventing a new mechanism.
- The secondary bug (silently resolving to an outer same-named class instead of
  raising `X::Undeclared::Symbols`) suggests the lookup falls back to the global
  type registry; a role-lexical name must shadow it.

## Repro files

`tmp/rolelexclass.p6` and `tmp/rolelexclass2.p6` (recreate from the snippets
above — `tmp/` is gitignored).
