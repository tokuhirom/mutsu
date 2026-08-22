# `constant NAME := Metamodel::ClassHOW.new_type(name => 'NAME')` immediately errors as "immutable"

Found by the doc-diff harness (`docs/doc-diff-backlog.md`, `Language/mop.rakudoc:34` —
the doc's own worked example showing what a `class A { ... }` declaration desugars to
at the metaobject-protocol level).

## Repro

```raku
constant A := Metamodel::ClassHOW.new_type( name => 'A' );  # class A {
A.^add_method('x', my method x(A:) { say 42 });             #   method x()
A.^compose;                                                 # }

A.x();
```

- `raku`: `42`
- `mutsu` (`target/debug/mutsu`): dies immediately on the `constant A := ...` line
  with `Cannot modify an immutable 'A' type object`.

## Minimal isolation

The error fires on the `constant` binding alone, before any `.^add_method`/`.^compose`
call:

```raku
constant Zorp := Metamodel::ClassHOW.new_type( name => "Zorp" );
say Zorp.^name;
```

- `raku`: `Zorp`
- `mutsu`: same `Cannot modify an immutable 'Zorp' type object` error.

## Root cause hypothesis

The guard that produces this error lives in `src/vm/vm_exec_dispatch.rs` (~line
1125-1140): when binding/assigning to a bareword name, it rejects the write if the
name is already registered as a class (`self.has_class(&name)`) and the env already
holds a `Package` value under that name. This guard is meant to catch real
reassignment attempts like `Foo .= new` on a frozen type object.

The false positive here is almost certainly that `Metamodel::ClassHOW.new_type(name =>
"Zorp")` — mutsu's implementation of `new_type` — has a **side effect of eagerly
registering a class named "Zorp"** in the class registry (and/or binding a `Package`
value under that bareword in env) *before* the `constant Zorp := ...` binding itself
executes. Then when the `constant` binding tries to actually establish `Zorp` as a
name, the guard sees "there's already a registered class + Package value called Zorp"
and treats it as an illegal reassignment — even though this is the *first and only*
write, not a mutation of an already-composed type.

In real Rakudo, `Metamodel::ClassHOW.new_type(...)` returns a fresh, still-mutable
type object; it does not itself bind any global/package name. The name only comes into
existence when the surrounding `constant`/`my`/`our` binding assigns it.

## Why this is deep

Fixing this requires understanding (and probably reworking) how mutsu's
`Metamodel::ClassHOW.new_type` interacts with the class registry and with the
generic "protect already-composed type objects from mutation" guard — a change that
touches class registration bootstrapping, not just the guard's condition. There's also
a design question of whether mutsu should support the *general* pattern of scripting
type creation directly through `Metamodel::*` calls (as opposed to only via `class`/
`role`/`enum` declarations), which has broader implications for the MOP.

## Affected files (starting point)

- `src/vm/vm_exec_dispatch.rs` (~line 1125-1140) — the immutable-type-object guard.
- Wherever `Metamodel::ClassHOW.new_type` is implemented (grep for `"new_type"`) —
  check whether it eagerly registers the class/binds the name as a side effect, and
  whether that should instead be deferred until an explicit `.^compose` (matching
  Rakudo's model of "mutable until composed").

## Related finding: `new_type` ignores which `Metamodel::*HOW` was called (2026-08-22, doc-diff batch-4)

`src/runtime/methods_instance_ops.rs` (~line 2138) dispatches `.new_type` generically
for *any* `Metamodel::*` package name
(`matches!(target.view(), ValueView::Package(n) if n.resolve().starts_with("Metamodel::"))`)
— it always registers a plain empty class and returns a bare `Package` value,
regardless of which specific metaclass (`ClassHOW`, `ParametricRoleHOW`,
`ParametricRoleGroupHOW`, ...) was actually invoked. This means the resulting type
object's `.HOW` always introspects as `ClassHOW`-shaped, even when the caller asked
for a different metaclass. Confirmed independently via
`Type/Metamodel/ParametricRoleGroupHOW.rakudoc:27` (`Metamodel::ParametricRoleHOW.new_type(...)`
also reports `.HOW` as `ClassHOW`) — same root cause and same generic `new_type`
handler as the already-filed
[metamodel-parametricrolehow-new-type-wrong-how.md](../tickets/metamodel-parametricrolehow-new-type-wrong-how.md),
which has the minimal repro; fixing `new_type` to actually respect (and record) which
`Metamodel::*HOW` it was called on is likely a prerequisite for all three findings
tracked across these two files, not just a message-text difference.
