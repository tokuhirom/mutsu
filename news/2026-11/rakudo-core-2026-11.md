# Rakudo-Core re-vendored from rakudo 2026.11

The modules mutsu ships from rakudo's own `lib/` are now the rakudo 2026.11
release's (they were 2026.06; #19210, item 1). Five of the eleven vendored
files changed upstream, and all five are copied verbatim:

- **`Test`**: four internal hunks.
  - `my %ENV := %*ENV` at the top, read for `RAKU_TEST_*`.
  - An explicit `my` on the role `is test-assertion` mixes in.
  - `done-testing` binds `$*OUT`/`$*ERR` to lexicals.
  - TAP output, diagnostics and every assertion's verdict are unchanged:
    `make roast`, all of `t/` and every battery suite were measured with the
    new file before the switch.
  - The `%ENV` bind exposed a mutsu slow path that re-interned names on every
    unit-lexical store, fixed first in #19215.
- **`NativeCall` / `NativeCall::Types`**:
  - A definiteness-constrained return type (`sub f(--> Str:D) is native`) maps
    to its base type and the returned value is checked: a NULL `char *` now
    throws `X::TypeCheck::Return` instead of returning `(Str:D)`.
  - mutsu already had what this needs (`Metamodel::DefiniteHOW.base_type`,
    `X::TypeCheck::Return`). The hand-built exception's message was fixed in
    #19211.
  - `explicitly-manage` now returns the managed string rather than its `CStr`
    box.
- **`NativeCall::Dispatcher`**: return and argument checks through MoarVM's
  `boot-resume` dispatcher. mutsu never loads this file (ADR-11203).
- **`Pod::To::Text`**: a parameter's declarator doc is no longer printed twice.

`t/nativecall/nativecall-type-surface.t` follows the new `explicitly-manage`
return. `t/nativecall/nativecall-definite-return-check.t` pins the `--> T:D`
return check against rakudo 2026.11's output.
