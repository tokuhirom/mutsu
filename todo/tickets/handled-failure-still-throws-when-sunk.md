# A `Failure` that `.defined` handled still throws when it is sunk

Calling `.defined` (or `.Bool`, `.so`, ...) on a `Failure` *handles* it: the
Failure stops being live, and sinking it afterwards is a no-op. mutsu marks it
handled for some purposes but the trailing-value check of a block does not
respect the mark:

```
$ raku  -e 'my $f = "foo"[2]; $f.defined; try { $f }; say (defined $!) ?? "died" !! "lived"'
lived
$ mutsu -e 'my $f = "foo"[2]; $f.defined; try { $f }; say (defined $!) ?? "died" !! "lived"'
died
```

No `EVAL` and no module are involved — `"foo"[2]` produces the `Failure`
directly. The same shape one level down also throws:

```raku
my &c = { my $f = "foo"[2]; $f.defined; $f };
try { c() }        # mutsu throws, raku lives
```

## Why it matters

rakudo's `Test.rakumod` writes `lives-ok` as

```raku
multi sub lives-ok(Callable $code, $reason = '') is export {
    try { $code(); }
    my $ok = proclaim((not defined $!), $reason) or _diag($!);
    ...
}
```

so any `lives-ok { ... }` whose block ends in a handled `Failure` is reported as
*died*. That is what keeps two assertions of
`t/statement-call-sinks-its-value.t` red under the aliased upstream module
(`todo/tickets/vendor-real-test-module.md`) while they pass under mutsu's native
provider and under `raku`.

## Where to look

`failure_to_runtime_error_if_unhandled` is the shared predicate; `OpCode::
ThrowIfFailure` (the block/routine trailing-value check, `vm_exec_dispatch.rs`)
and `OpCode::SinkPop` both call it. The question is whether `.defined` on a
`Failure` actually sets the handled flag on the *same* value the caller still
holds, or on a clone — mutsu's `Value` is `Arc`-backed, so a method that
reconstructs the instance rather than mutating it in place would leave the
original live. Check `.Bool` / `.so` / `.not` / `defined($f)` (the sub form)
too; raku handles the Failure for all of them.
