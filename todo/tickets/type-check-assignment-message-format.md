# `X::TypeCheck::Assignment` from a typed `my` declaration uses a short message

Assigning a non-conforming value to a typed lexical produces a message that
differs from raku in two ways: it uses `expected X, got Y` instead of
`expected X but got Y (repr)`, and it does not append the offending value's
short representation.

## Repro

```raku
try { my Int $x = "s" };  say $!.message;
class C {};
try { my C $y = 3 };      say $!.message;
```

```
raku:   Type check failed in assignment to $x; expected Int but got Str ("s")
        Type check failed in assignment to $y; expected C but got Int (3)
mutsu:  Type check failed in assignment to $x; expected Int, got Str
        Type check failed in assignment to $y; expected C, got Int
```

The same short form appears on the `.new` attribute path:

```raku
class Foo { has Int $.n }
try { Foo.new(n => "s") }; say $!.message;
# raku:  Type check failed in assignment to $!n; expected Int but got Str ("s")
# mutsu: Type check failed in assignment to $!n; expected Int, got Str
```

## Root cause

`runtime/utils/errors.rs::type_check_assignment_error` already builds the
correct raku wording, but several call sites format their own message with the
short template instead of going through it:

- `src/value/error_typed.rs:255` and `:260` (the `X::TypeCheck::Assignment`
  rendering used by the typed-lexical path)
- `src/runtime/methods_object_attr_constraints.rs:148`, `:264`, `:292`
  (`enforce_attribute_where_constraints` and the attribute-default checks)

## Why it is not a one-line change

The message text is load-bearing for a large number of roast assertions and for
`t/` pins that were written against the current short form. Unifying on the raku
wording means sweeping every expectation that matches on `expected .*, got` —
worth doing, but it needs its own PR with a full roast run, not a drive-by edit.

## Related

A second, smaller divergence sits on the same paths: a subset declared inside a
package is reported unqualified here (`expected RM`) even though
`get_attr_type_constraint` now resolves it (`expected Foo::RM`) on the attribute
paths — the typed-lexical path has no owner to resolve against and would need to
follow the env alias instead. See
`news/2026-07/subset-package-qualified-name.md`.
