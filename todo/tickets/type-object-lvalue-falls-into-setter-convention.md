# A type-object method lvalue silently succeeds through the legacy setter convention

`$Class.m($arg) = $v` where `m` is **not** rw-capable must die in raku
(`Cannot modify an immutable Int (42)`). mutsu reports success and drops the
write, after calling `m` with the assigned value — or, for a sigilless
parameter, with the *invocant* — as its argument.

Measured 2026-09-05 on `main` + ADR-0067 slice 2 (debug build):

```
$ raku  -e 'class N { method m($x) { say "called with ", $x; $x } }; my $a = 42; N.m($a) = 5; say $a'
called with 42
Cannot modify an immutable Int (42)

$ mutsu -e 'class N { method m($x) { say "called with ", $x; $x } }; my $a = 42; N.m($a) = 5; say $a'
called with 5          # the assigned value bound into the method's first parameter
42                     # exit 0: the assignment silently did nothing

$ mutsu -e 'class N { method m(\x) { say "called with ", x; x } }; my $a = 42; N.m($a) = 5; say $a'
called with (N)        # a sigilless param re-reads by source name and binds the INVOCANT
42
```

The **instance** twin is already correct — `N.new.m($a) = 5` raises
`X::Assignment::RO: method 'm' is not rw`.

## Root cause

`Interpreter::assign_method_lvalue_with_values`
(`src/runtime/methods_mut_method_lvalue.rs`) tries the ADR-0059 lvalue return
first, then falls through to mutsu's legacy `$obj.name($value)` setter
convention at the `call_method_mut_with_values(var_name, target, method,
vec![value])` site. That site is guarded by
`setter_convention_would_preempt_lvalue_return`
(`src/runtime/lvalue_container_return.rs`), which **deliberately returns `false`
for a non-instance target**: for a type object the lvalue return has already run
at the top of the function and declined, so the doc comment reasons that "the
remaining legacy chain is all that is left to try and must not be blocked".

That reasoning holds for the shapes the legacy chain genuinely serves, but it
also lets a plainly non-rw-capable method be re-called as a setter, and the
`Ok(result) => return Ok(result)` arm then reports the assignment as done. The
instance path never reaches this because it hits the
`"cannot assign through .{method} on non-instance"` / `"method '{m}' is not rw"`
refusals instead.

## Why this is not a one-line fix

Blocking the setter convention for a type-object invocant would change every
`Class.name($v)`-shaped lvalue in the corpus at once, and the legacy convention
has no declaration-level oracle to gate on the way the instance path does
(`Interpreter::method_is_rw_capable`, added by ADR-0067 slice 2). The honest fix
is to decide what the type-object legacy chain is still *for* — enumerate its
real consumers in `t/` and roast — and then either gate it on the same
rw-capability oracle or retire it. Both need a measurement pass first, which is
why this is a ticket rather than part of slice 2.

## Pin

`t/method-rw-capability-oracle.t` asserts the weaker fact that currently holds
(the caller variable is untouched) and points here, so the row becomes a real
refusal assertion when this is fixed.
