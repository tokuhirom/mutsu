# A native method cannot return its invocant's container, so `$x.VAR = ...` / `.snitch = ...` fail

Split off from `news/2026-08/snitch-method-unimplemented.md`, where the five
other documented `snitch` examples were fixed and this one was not.

## Repro

```raku
my $a = 42;
$a.VAR = 5;
say $a;
```

- raku: `5`
- mutsu: `X::Assignment::RO: cannot assign through .VAR on non-instance`

The same shape, from `raku-doc/doc/Type/Any.rakudoc:1550`:

```raku
use v6.e.PREVIEW;
(my $a = 42).snitch = 666; say $a;   # raku: 42 then 666; mutsu: the same error
```

## Root cause

mutsu's assign-through-a-method path
(`src/runtime/methods_mut_method_lvalue.rs`) is **attribute-backed**: it locates
a public attribute (or a role mixin entry) on an `Instance` receiver and writes
into it. A user-declared `method p() is rw { return-rw $!v }` therefore works,
because it resolves to an attribute in the end.

A *native* method has nowhere to put the answer. `.VAR`, `.snitch` and friends
are `is rw` in rakudo because they return the invocant's **container**, and in
mutsu the receiver reaching the lvalue path is already the decontainerized value
(`Int 42`), which is why the error is literally "on non-instance". There is no
way for a native method to hand back a `ContainerRef` that the assignment could
write through.

## Why it is not a small fix

Making this work means keeping `ContainerRef` alive across method dispatch —
the receiver must arrive as a container, the native method must be able to
return it, and the assignment path must recognise a returned container as an
lvalue. That is exactly the "make `ContainerRef` deref universal" work
(ADR-0001 §2.1 / Track B, now unblocked by
[ADR-0013](../../docs/adr/0013-container-interior-mutability-cellvalue.md) §7),
not a per-method patch. Adding a special case for `.snitch` alone would be a
band-aid over a general gap.

## Affected files (starting point)

- `src/runtime/methods_mut_method_lvalue.rs` — the "cannot assign through .{} on
  non-instance" site
- `src/vm/vm_call_method_compiled.rs` — the compiled twin of that check
- `src/runtime/methods_io_dispatch.rs` — `dispatch_snitch`, which would return
  the container once one is reachable
