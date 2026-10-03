# `Code.$!do` is readable and rebindable

`nqp::getattr($code, Code, '$!do')` now answers the body a code object runs. The answer is itself
callable. `nqp::bindattr($routine, Code, '$!do', $body)` replaces what every later call of the
routine runs: a named call, a `&f` call, a call through a copied code value, a wrapper's
`callsame`. The routine keeps its identity, name, signature and mixed-in roles. Before this change,
the read returned `Nil` and the bind did nothing.

```raku
use nqp;
sub a { "a" }
my $b := -> { "b" };
nqp::bindattr(&a, Code, '$!do', nqp::getattr($b, Code, '$!do'));
say a();    # b
```

Upstream NativeCall's backend-neutral path needs this. It replaces an `is native` routine's body
with a closure that calls `nqp::nativecall` (ADR-11203, #11207). Together with the FFI ops from
#11211, the vendored upstream module, loaded by `scripts/nativecall-upstream-trial.sh`, now calls
`strlen` through `sub strlen(Str --> size_t) is native {*}` and round-trips a `CStruct`. Only the
`CArray` steps (#11209) still fail.

A bound body is the innermost entry of the routine's `.wrap` chain, under a reserved handle.
Rakudo's `.wrap` is itself a `$!do` rebind, and that chain is already what every call path of a
routine honours, including the name-keyed call caches, TRIR and the JIT. So no call path changed:

- Wrappers added later still run around the bound body, and their `callsame` reaches it.
- The declared body is no longer reached.
- A `$!do` value is a direct code object: calling it never re-enters a wrap chain.
- `.wrap` and the new bind share one key computation, `routine_wrap_key`.

Rakudo derives `Code.name` from `$!do`'s codename. mutsu keeps the routine's own name (#11462).
