# `method name() is export` (non-operator name) registers no importable sub

## Root cause

`class_body_method_decl` (and, as of ADR-0019 D3-6, `augment_class`'s method
arm and the role walker) handle `is export` on a method in two different ways
depending on the method's name:

- An *operator-categorical* name (`prefix:<~>`, `infix:<as>`, ...) goes through
  `register_exported_operator_method_sub`, which builds and registers a real
  `FunctionDef` sub-form under `{class}::{op}` in `self.registry_mut().functions`.
  `import_module`'s sub-export loop reads exports from that registry and this
  path works correctly (confirmed against `raku`).
- Any other (plain) method name instead calls `register_exported_var(class,
  "&name", tags)`, which only records `class::&name` as an *exported name* in
  `exported_vars` — it never creates a corresponding sub value. `import_module`'s
  var-export loop then does `self.env.get("{sigil}{module}::{bare}")` looking
  for an actual value at that key, finds nothing (no such env entry was ever
  written), and silently imports nothing.

## Minimal repro

```raku
class Foo {
    method greet() is export { "hi" }
}
import Foo;
say greet(Foo.new);
```

`raku` prints `hi`. mutsu (as of 2026-08-08, `main`) fails with `Unknown
function: greet`. Reproduces identically whether the method is declared in the
class body or synthesized via `augment class Foo { ... }` — this is not
walker drift, both the class walker and `augment_class` share the same broken
`register_exported_var` path.

## Why this is a separate ticket from the operator-method case

The operator-method form is real and tested (`t/` + roast coverage rely on
`method infix:<as> is export` style declarations, e.g. for HTTP::UserAgent-style
operator overloads). The plain-name form has no working implementation at all
today, not a drift between walkers, so fixing it means making
`register_exported_var`'s plain-method path do what
`register_exported_operator_method_sub` does for operators: build a real
`FunctionDef` (self as first positional, forwarding to the method) and install
it under `{class}::{name}` so `import_module`'s sub-export loop (not the
var-export loop) can find and copy it. That is a real feature addition, not a
one-line fix.

## Affected files

- `src/runtime/registration_class_body_method.rs` (`is_export` handling)
- `src/runtime/registration_role_method.rs` (same shape, if it handles
  `is_export` at all — unconfirmed, not checked as part of this finding)
- `src/runtime/registration_class_augment.rs` (added by ADR-0019 D3-6)
- `src/runtime/runtime_module_exports.rs` (`register_exported_var`,
  `import_module`'s var-export loop)
