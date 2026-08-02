# A type declared in a class body is scoped to that body

A `my class` / `role` / `subset` declared inside a `class` body registered its
short name into the *current* env and left it there, so a same-named class at
file scope stopped being reachable for the rest of the program:

```raku
class Foo { method who() { "outer-Foo" } }
class InClass {
    my class Foo { method who() { "inner-Foo" } }
    method make() { Foo.new }
}
say InClass.make.who;   # inner-Foo   (both)
say Foo.new.who;        # raku: outer-Foo   mutsu was: X::Undeclared::Symbols: Foo
say Foo.^name;          # raku: Foo         mutsu was: (never reached)
```

## Root cause

`exec_register_class_op`'s nested-type branch did three global, permanent things:

```rust
self.suppress_name(&resolved_name);
self.register_class_scoped_short_name(&resolved_name);
env.insert(resolved_name.clone(), Value::package(Symbol::intern(&storage_name)));
```

A class body is not an env scope in mutsu — `register_class_decl` deliberately
does not restore `env` on success — so nothing put the outer binding back. On top
of that, `exec_get_bare_word_op` checked `is_name_suppressed` *early*, before any
other resolution route, and treated a `Package` env value as not counting as a
local declaration. That made the suppression a poison pill for the short name
rather than a statement about type visibility.

## Changes

- `src/runtime/registration_class_decl.rs`: when a class body finishes, each bare
  short name it bound to one of its own nested types (`Package("Outer::Inner")`,
  or the mangled `Outer::Inner\0<decl-id>` storage form) is restored to whatever
  the enclosing scope had — removed if there was nothing. The class's own methods
  do not need the binding: `resolve_suppressed_type` resolves a nested short name
  through the owner package chain.
- `src/vm/vm_typedecl_ops.rs`: a **role** or **subset** declared in a class body
  is now recorded with `register_class_scoped_short_name`, which is what gates
  that probe. Only nested classes were recorded before, so
  `unit class HTTP::UserAgent; role Connection { … }` — whose methods further down
  write `my Connection $conn` — depended entirely on the leaked env binding, and
  `class Req { subset Method of Str … }` likewise.
- `src/vm/vm_var_get_ops.rs`: the suppressed-name error moved to the **end** of
  the resolution chain. Suppression means "this short name is not visible as a
  *type* out here", not "poison the name for every kind of symbol", so a later
  declaration in an inner scope — an enum member, a sub, a constant — still wins,
  and a name that resolves some other way is never rejected.

Pinned by `t/class-body-type-scope.t` (7 cases across class, role and subset),
which passes identically under `raku`.

## What is still open

The *package* half of the same problem is filed as
`todo/tickets/package-short-name-alias-is-global.md`: `class A::B` still binds the
bare name `B` globally instead of into package `A`. That is the live blocker
behind `Cro::HTTP::ResponseParser` / `RequestParser`, whose `my enum Expecting
<StatusLine Header Body>` loses to `Cro::HTTP::Header`'s short name. Gating the
alias on the declaring package is not enough — `URI` declares `class URI::Path` at
file scope and `unit class URI`'s methods name it bare — so it needs a real
package-scoped alias plus a method-dispatch `current_package` anchor.
