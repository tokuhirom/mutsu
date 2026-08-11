# `.$name` (unquoted dynamic method call) should require `$name` to be Callable/type-object/CALL-ME, not accept a bare string

## Repro

```raku
my $m = "uc";
say "hi".$m();
# raku:  No such method 'CALL-ME' for string 'uc'
# mutsu: HI  -- silently accepts it as string-name dispatch
```

Only the **quoted** form (`."$name"()`, the `.""` operator -- see
`raku-doc/doc/Language/objects.rakudoc:458-468`, "Method names can be resolved at runtime with the
`.""` operator") is the general indirect-by-string-name dispatch. The unquoted `.$name` form
requires the name-value to be Callable, a type object (which dispatches by its own short name,
e.g. `$str.$Int` calls `.Int`), or otherwise support `CALL-ME` -- a bare `Str` in `$name` is none
of those, and real raku raises `X::Method::NotFound` naming the missing `CALL-ME` method.

## Root cause

`Interpreter::dynamic_method_name` (`src/vm/vm_call_method_mut_ops.rs:23-28`) falls back to
`.to_string_value()` for *any* non-`Package` name value:

```rust
fn dynamic_method_name(name_val: &Value) -> String {
    match name_val.view() {
        ValueView::Package(name) => name.resolve(),
        _ => name_val.to_string_value(),
    }
}
```

So `.$m()` and `."$m"()` compile to the identical AST node (`Expr::DynamicMethodCall`) and behave
identically in mutsu, accepting a program shape raku rejects at the unquoted call site.

## Why this is a ticket, not a fix-now

Fixing it correctly means threading whether the *original call syntax* was quoted (`.""`) or
unquoted (`.$name`) through to `dynamic_method_name`/`exec_call_method_dynamic_op`, and for the
unquoted case, raising `X::Method::NotFound` (naming `CALL-ME`) when the name value is a bare
scalar with no `CALL-ME` method -- narrower than a full redesign but touches parser/compiler AST
shape (does `Expr::DynamicMethodCall` currently carry enough info to distinguish the two source
forms?) as well as the VM handler, so it needs its own scoped investigation rather than a drive-by
patch.

## Where found

Discovered during ADR-0019 E5c (`CallMethodDynamic` classification,
`todo/deep/adr0019-e5-e7-entry-routing.md` §"E5c") while raku-verifying representative
`CallMethodDynamic` call shapes. Unrelated to the E5 native-vs-user dispatch-ordering campaign
itself -- a pre-existing "which name values are legal" gap upstream of dispatch routing.
