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

## Deep-triage update (2026-08-31)

Moved from `todo/tickets/` after rechecking the current ADR outcomes.
ADR-0001 §7 says its former Track-B/GC coupling is historical: first-class
element cells now need an independently justified design. ADR-0013 §7 solved
the interior-mutability safety prerequisite, but it does not define how a
receiver `ContainerRef` survives every method-dispatch and lvalue-assignment
path. This item therefore needs a bounded design campaign for universal
container-reference propagation (including native-method returns) before an
implementation slice can be selected. The existing `.VAR` and `.snitch`
repros remain the acceptance cases; do not special-case either method.

## Affected files (starting point)

- `src/runtime/methods_mut_method_lvalue.rs` — the "cannot assign through .{} on
  non-instance" site
- `src/vm/vm_call_method_compiled.rs` — the compiled twin of that check
- `src/runtime/methods_io_dispatch.rs` — `dispatch_snitch`, which would return
  the container once one is reachable

## Re-verified 2026-09-01 (TRIAGE regeneration)

**The `.VAR` row's raku expectation above is wrong.** `raku -e 'my $a = 42;
$a.VAR = 5; say $a'` does NOT print `5` — it dies with `Cannot assign to a
readonly variable or a value`. Both implementations refuse; only the wording
differs (`X::Assignment::RO: cannot assign through .VAR on non-instance` in
mutsu). So `$a.VAR = 5` is not a divergence and must not be used as an
acceptance case.

The `.snitch` row is the real one and still reproduces: `use v6.e.PREVIEW;
(my $a = 42).snitch = 666; say $a` prints `42` / `666` in raku and dies with
the same RO error in mutsu. Keep `.snitch` as the acceptance case.
