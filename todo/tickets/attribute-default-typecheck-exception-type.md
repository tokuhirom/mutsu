# A `:D`/`:U` attribute-default type-check raises the wrong exception type

Found 2026-07-25 while unifying the `X::TypeCheck::Assignment` wording
(`news/2026-07/typecheck-assignment-message-parity.md`). That slice fixed the
message on every ordinary assignment path; the attribute *default* check was
left because it needs an exception-type change, not a wording tweak.

## Repro

```raku
class A { has Int:D $.n = Int }
try { A.new };
say $!.^name;
say $!.message;
```

```
raku:   X::TypeCheck::Assignment
        Type check failed in assignment to $!n; expected Int:D but got Int (Int) (perhaps Nil was assigned to a :D which had no default?)
mutsu:  X::TypeCheck::Attribute::Default
        Type check failed in default value of attribute $!n; expected Int:D, got Package
```

Three things differ: the exception type, the whole message template, and the
reported "got" type (`Package` where raku says `Int` — mutsu names the internal
representation of a type object rather than the type).

## Why it is not a one-liner

`X::TypeCheck::Attribute::Default` is a real Rakudo exception, raised for a
*different* situation (a default that can never satisfy the constraint, caught at
compile time — `Can never assign default value …`). Making the `:D`-unsatisfied
case raise `X::TypeCheck::Assignment` therefore is not "rename the type": the two
cases have to be told apart first, and the compile-time one is not implemented at
all. The `got Package` half is separate again — it is the type-object naming used
by `value_type_name`, which other messages may depend on.

## Affected files

- `src/runtime/methods_object_attr_constraints.rs` — the `:U` and `:D` arms
  (the two `X::TypeCheck::Attribute::Default` constructions)
- `src/runtime/utils/errors.rs` — `type_check_assignment_typed_error` is the
  builder the `:D` path should end up using

## Impact

Cosmetic unless a program matches on the exception type. No roast test currently
asserts either form.
