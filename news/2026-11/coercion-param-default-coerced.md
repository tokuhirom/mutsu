# A coercion-typed parameter's default is coerced

`sub f(P $a, Str() $b = $a)` bound `$b` to the `P` object itself when the caller passed nothing: the
default was only type-checked against the coercion's target, never converted. A default is now coerced
like a supplied argument, so `$b` is `$a.Str`. This is what IdClass's `-> Id1() $a, Str() $b = $a`
pointy blocks rely on; its `t/06-role.rakutest` still waits on a separate closure-binding bug.
