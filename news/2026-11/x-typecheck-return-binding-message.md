# Hand-built `X::TypeCheck::Return` and `X::TypeCheck::Binding` have their messages

A hand-built `X::TypeCheck::Return.new(:got, :expected)` answered `.message`
with a "Stub code executed" failure. Thrown, it read
`Type check failed in ;`, which is the parent `X::TypeCheck` template with an
empty `operation`. `X::TypeCheck::Binding` had the same gap.

The interpreter's own throws store a pre-built message, so only user and
module code ever hit this. That includes rakudo 2026.11's `NativeCall`, which
checks a `--> T:D` native return that way (#19211, found reviewing #19210).

The two subclasses now word their messages as rakudo does:

- `X::TypeCheck::Return`: `Type check failed for return value; expected … but got …`
- `X::TypeCheck::Binding`: `Type check failed in binding; …`

Both share `X::TypeCheck`'s `expected … but got …` tail.
