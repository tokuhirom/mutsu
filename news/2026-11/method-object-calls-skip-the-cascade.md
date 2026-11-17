# Method-object calls and Proxy `.VAR` private calls skip the interpreter cascade

Invoking a `Method` object on an invocant, as in `$obj.$m(...)` or
`$m($obj, ...)`, is a common reflective idiom. AttrX::Mooish's `invoke-opt`
does `$invocant.$method(|$params)` on every lazy build, for example. Each such
call used to go through the interpreter's `call_method_with_values` cascade
twice:

- once to reach the Method object's `CALL-ME`, at the tail of the instance
  cascade;
- once more for the owner-qualified call `$obj.Owner::name` that `CALL-ME`
  re-dispatches to.

The value-call entry now calls the Method object's invocation directly. The
owner-bound call then goes straight to the cascade's qualified-call arm, which
is now its own function (`dispatch_qualified_call`). Both shortcuts apply only
to a method a user class declares. A core type's method object
(`Mu.^find_method('perl')`, which is `self.raku`) keeps the cascade. So does
any call while a wrap chain or a user `^find_method` exists.

A plain `$o.$m()` dropped from 104k to 46k instructions (callgrind slope,
profiling build).

Separately, a private method called on a Proxy subclass's `.VAR` object
(`callwith(...).VAR!SET-SELF(%h)`, Mooish's `AttrProxy.new`) is now answered
by the early proxy lane instead of the end of the cascade. That saves only
~4k instructions per call: most of that call's cost is in the method body.
Its attribute binds on a Proxy `self` cost ~18k each.

On the single-threaded loop of AttrX::Mooish `t/090-concurrent`, the change
is worth about 1%: 6.92M → 6.85M instructions per iteration.

Found on the way and filed: #18049. Calling a *wrapped* Method object as a
value skips its wrapper, on `main` as before.
