# A `:=` rebind of an outer lexical bound to an immutable value no longer dies

A lexical bound straight to an immutable value (`my $x := 42`, or a package
lexical bound to an `nqp::list_i`) could not be re-bound from a sub that sees it
as an outer:

```raku
my $y := 5;
sub ry() { $y := 2 }
ry();   # rakudo: rebinds -- mutsu: "Cannot assign to an immutable value"
```

A statement-level scalar rebind compiles to `MarkScalarBindContext` +
`MarkRebindContext` + the store. For a name outside the running frame the store is
`SetGlobal`, and its readonly check skipped only the `MarkBindContext` case (a
container `@`/`%` rebind), not the rebind marker. So the `Immutable` mark that
`my $y := 5` left behind rejected the rebind as if it were an assignment.
`SetGlobal` now skips that check for a rebind of a name whose mark is exactly
`ReadonlyKind::Immutable`, since a rebind replaces the binding and not the value
inside it. Every other kind still refuses the rebind: a `constant`, a sigilless
term, a signature-bound parameter (`roast/S04-declarations/constant.t` and
`t/vm/binding/constant-rebind.t` pin the `constant` case). Assigning to the name
afterwards still dies, because it is bound to an immutable value again.

Pinned by `t/vm/binding/bind-outer-immutable-rebind-from-sub.t`. Closes #9238.

While testing this I found that a rebind never updates the name's readonly
state. `$w := 42; $w = 5` assigns where rakudo dies, and a sub's `$y := $z`
leaves `$y` immutable. This comes from the name-keyed, frame-journaled
`readonly_vars` mark and is filed as #9277.
