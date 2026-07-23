# A qualified call `self.Builtin::method` reaches a native ancestor method

A qualified method call whose qualifier is a *native* builtin ancestor — e.g.
`self.IO::Path::slurp` from a class that `is IO::Path` — did not work. mutsu's
qualified-dispatch paths only resolved a **user**-defined ancestor method, so a
native (Rust-implemented) method of the qualifier type was never found:

- on a plain subclass instance the call fell through to
  `No such method 'IO::Path::slurp'`;
- on a run-time mixin (`self but SomeRole`) it fell through to the non-instance
  path, which split the name at the *first* `::` (`IO` / `Path::slurp`) and died
  with `Cannot dispatch to a method on IO`.

Now, after user-method lookup fails, both the instance and mixin qualified-dispatch
paths route to the native handler for the qualifier
(`call_native_instance_method`), provided the qualifier is in the receiver's MRO.
Dispatching to the qualifier — not the receiver's most-derived class — means an
overriding `method slurp` that calls `self.IO::Path::slurp` gets the parent's
native version rather than recursing into itself. A qualifier that is not in the
MRO still dies with `X::Method::InvalidQualifier`, and qualified calls to a
user-defined ancestor method are unchanged.

This fixes the `IO::Path::AutoDecompress` distribution (T-057), whose `Proccer`
role delegates to `self.IO::Path::slurp` / `self.IO::Path::lines`;
`t/01-basic.rakutest` now passes 8/8. Pin: `t/qualified-native-ancestor-method.t`.
