# A bare Numeric method no longer provides .Str

A class that defined a `Numeric` method (without `does Real`/`does
Numeric`) got its `.Str` — and `~`, string interpolation, every
stringifying context — routed through that method. In rakudo `Str` falls
back to `Mu.Str` regardless; a `Numeric` method has no bearing on it. The
same numeric-only class still bridges numeric-context coercion (`+1` etc.)
to it, matching rakudo.

The fallback chain in `methods_instance_ops.rs` gated numeric-bridge
delegation on `has_user_method(..., "Numeric")` alone, which is true for
any class merely declaring a method of that name. Stringify methods
(`Str`/`Stringy`) now require the object to genuinely compose `Real` or
`Numeric` — checked via `does_check` (mixin composition) and
`class_does_role` (static class-level composition, e.g. `class P does
Real { ... }`, which `does_check` alone cannot see since it has no
interpreter/registry access) — before bridging to the number; numeric
coercion methods keep the bare-method exception since that matches
rakudo (`class P { method Numeric { 7 } }; P.new + 1` is `8` there too).

See [#8153](https://github.com/tokuhirom/mutsu/issues/8153) and the
regression test `t/oo/method/numeric-method-does-not-provide-str.t`.
