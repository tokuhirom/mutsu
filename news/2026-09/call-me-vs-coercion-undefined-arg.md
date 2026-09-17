# CALL-ME dispatch no longer preempted by the coercion-type-term shortcut

`Foo($x)` on a type object that declares `CALL-ME` must dispatch to it —
never to the "calling a type with a type object argument constructs a
coercion type object" shortcut (`Str(Any)`, `Int(Str)`, `Child(Parent)`).
That shortcut, in `call_function_fallback`, fired whenever the single
argument's runtime value was itself a type object or undefined (`Nil`),
regardless of whether the target class declared `CALL-ME` at all. It ran
*before* the later branch that correctly implements "CALL-ME wins over
COERCE/new", so a class like the `Trap` zef distribution's
`method CALL-ME(Trap:U: $one is raw) { $one = self.new(:$tee) }`, called
with a not-yet-defined argument (`Trap(my $*OUT)`, where `$*OUT` starts out
`Any`), never ran its `CALL-ME` method at all — it silently got back the
symbolic type object `Trap(Any)` instead, which then died with
"No such method 'text'" the moment the caller tried to use it.

The fix gates that shortcut on `!class_has_method(name, "CALL-ME")` /
`!role_has_method(name, "CALL-ME")`, resolving a lexically-scoped (`my
class`) name's mangled storage identity first so the check also works for a
`my class Foo { method CALL-ME { ... } }` declared inside the current
lexical scope. Pinned by
`t/oo/class/call-me-undefined-arg-wins-over-coercion.t`.

This was found while working the `Trap` distribution
([tokuhirom/mutsu#7884](https://github.com/tokuhirom/mutsu/issues/7884) lock
board) — its `t/01-basic.rakutest` went from an outright die to running to
completion. A second, deeper bug in `$*OUT`'s sigilled/sigilless env-key
mirroring (documented in
[tokuhirom/mutsu#8645](https://github.com/tokuhirom/mutsu/issues/8645))
still keeps both of `Trap`'s baseline files at `partial` rather than
`green`.
