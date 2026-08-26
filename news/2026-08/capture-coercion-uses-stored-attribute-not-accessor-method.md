# `.Capture` on an object reads its public accessors, not its attribute store

`Mu.Capture` returns a `Capture` whose named arguments are the object's **public
attributes**, and each one is read through its accessor *method* — so an explicit
`method bar { … }` that overrides the auto-generated accessor is what the
Capture reports:

```raku
class Foo {
    has $.foo = 42;
    has $.bar = 70;
    method bar { 'something else' }
}.new.Capture.say;   # \(:bar("something else"), :foo(42))
```

mutsu printed `\(:bar(70), :foo(42))`.

## Root cause

`value_to_capture` (`src/builtins/methods_0arg/coercion.rs`) is a pure
value-level function with no interpreter, so its generic-object arm could only
iterate the raw attribute store:

```rust
ValueView::Instance { attributes, .. } => {
    let mut named = HashMap::new();
    for (k, v) in attributes.as_map().iter() { named.insert(k.resolve(), v.clone()); }
    …
}
```

That has two defects, not one: it bypasses method resolution entirely (so an
overriding accessor is invisible), and it exposes **private** attributes — a
`has $!hidden` showed up in the Capture as `:hidden(9)`.

## Fix

`try_interpreter_capture` (`src/runtime/methods_call_dispatch.rs`) — the existing
interpreter-aware `.Capture` hook that already handles `Channel`/`Supply`
draining and non-`Str` pair keys, and which runs *before* the native fast path —
gained an `Instance` arm. It reads the class's declared attributes via
`collect_class_attributes`, keeps only the public ones, and dispatches each
accessor through `call_method_with_values`, so overrides win and private
attributes never appear.

The arm returns `None` (falling back to the pure path) when the class declares no
public attributes, which is exactly what keeps every built-in whose `.Capture`
has its own spec — `Match`, `Duration`/`Instant`, `IO::Path`, `Signature`,
`Failure`, the `Blob`/`Buf` family — on their existing dedicated arms.

## Verified

`t/attribute-container-identity.t` asserts the override is dispatched, the plain
accessor still reports its value, a private attribute is absent, and the Capture's
key set is *exactly* the public attributes. It passes under both `raku` and
mutsu.
