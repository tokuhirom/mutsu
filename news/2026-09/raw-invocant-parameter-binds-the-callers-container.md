# A raw invocant parameter binds the caller's container

`class C { method m(\S:) { S = 7 } }; my $c = C.new; $c.m` leaves `$c` holding
`7` in raku, because a raw invocant parameter binds the caller's Scalar
container rather than a copy of its contents. mutsu left `$c` untouched. This is
ADR-0067 slice 3b, the inbound mirror of slice 3a: 3a made an lvalue *call* hand
a container back, this makes the invocant *arrive* as one.

Now identical to raku for a named receiver, whichever raw spelling is used and
whichever of the two compiled-method binders runs:

```raku
class A { method m(\S:)        { S = 7 } }; my $c = A.new; $c.m; say $c;  # 7
class B { method m($s is raw:) { $s = 7 } }; my $c = B.new; $c.m; say $c; # 7
class C { method m($s is rw:)  { $s = 7 } }; my $c = C.new; $c.m; say $c; # 7

use v6.e.PREVIEW; use MONKEY-TYPING;
augment class Int { method inc(\S:) { S = S + 1 } }
my $a = 42; $a.inc; $a.inc; say $a;                                      # 44
my @a = 1, 2; for @a <-> $e { $e.inc }; say @a;                          # [2 3]
```

## What the ADR got wrong, and measurement caught

**`is raw` on the *routine* is not part of this contract.** The ADR carried
slice 3a's rule forward and said raku needs the invocant raw *and* the routine
rw-capable, with `E1`/`E2` as regression controls for both halves. Re-measured
against raku v2026.07, that conjunction is the *outbound* contract only: `is
raw`/`is rw` on a routine answers "is this call an lvalue", while rawness of the
invocant *parameter* answers "does the body's write reach the caller". All three
programs above mutate with no routine trait anywhere, and a plain `$s:` or
`C $s:` invocant is refused whether or not the routine is `is raw`. So the
shipped oracle, `Interpreter::method_binds_raw_invocant`, is slice 3a's resolve
with the rw-capability conjunct dropped — both read the one
`method_def_has_raw_invocant` predicate, so the two halves cannot drift apart
about what "raw invocant" means.

**The recorded call chain was one of two, and neither was named at its top.**
The ADR's `rust-gdb` trace concluded 3b "is a signature change across that whole
chain". Re-tracing under `rust-gdb` found two disjoint chains — an `Instance`
receiver reaches `call_compiled_method` directly, while an `augment`ed native
receiver detours through `call_method_mut_with_values` →
`call_method_with_values` → `try_dispatch_compiled_method_direct_as` — and the
second runs through `call_method_with_values`, which is called from ~everywhere
and carries no source channel. What both chains share is their single origin,
the `CallMethodMut` opcode, which is also the only place that knows the
receiver's source name. So the transport is a one-slot channel armed at that
opcode and consumed at the binder, not a parameter: it is disarmed immediately
after the dispatch, and a binder only consumes it when it agrees on the method
name *and* is looking at a `ParamDef` that really is a raw invocant — which also
makes the actually-bound candidate the authority when multi-dispatch lands
somewhere the gate did not.

## Reuse, not new machinery

The container itself comes from slice 3a's `capture_lvalue_invocant_cell`,
reused verbatim. That is the point: its route order — an existing frame cell,
then an existing env container, then a direct slot box, and only then a freshly
minted one — is the rule slice 3a learned the hard way, and reusing it is what
makes `for @a <-> $e { $e.m }` bind the element's *already promoted* cell rather
than shadowing it with a disconnected second one. Write-through needs no new
consumer either: a local slot holding a `ContainerRef` already stores through
the cell and `GetLocal` already derefs one, so the slice adds no writeback path
and no new opcode. Only the *parameter* is boxed — the invocant value `base`
stays plain, so `self`, attribute seeding and every downstream branch that
matches `Instance`/`Array`/`Hash` see exactly what they saw before.

## Cost

The gate runs on every `$var.method(...)`, an order of magnitude more traffic
than slice 3a's `$obj.attr = v`, so slice 3a's `self.registry()` pre-filter (an
`RwLock` read acquisition) was not affordable. The flag is mirrored into a
process-global, set-only `AtomicBool` raised by the same single writer, with a
`debug_assert` that fails the debug `t/` suite if a future writer of the registry
field bypasses it. A program that declares no raw-invocant method pays one
relaxed atomic load per method call. Same-binary env-switch A/B on a release
build over 4M `$p.bump` calls: 15.15s with the gate against 15.49s with it
skipped, and 16.88s against 17.14s on a second round. The gate measures
*faster* both times, which is how "below the noise floor" looks — the sign is
meaningless and the between-round drift dwarfs the difference. For comparison,
slice 3a's un-filtered gate showed a clean, repeatable +13% on the same kind of
A/B, so this harness does surface a real cost when there is one.

## Still out of scope, and why

A *subscript* receiver (`@a[0].mut`, `%h<a>.mut`) is unchanged. It compiles to a
plain `CallMethod`, which carries no receiver name, and the `Index` op has
already read the element's value off the array — so there is neither a name to
box nor a location on the stack. What it needs is a producer (the subscript
handing over the element's own cell), and emitting one is an unconditional
compile-side change to a very common shape that has to be paired with a
decontainerize chokepoint and re-measured, including under the JIT. That is
`todo/tickets/subscript-receiver-raw-invocant-producer.md`. The
attribute-accessor receiver (`$d.v.mut`) is the same missing-producer problem
and belongs to slice 3a's E6 row. Both are unchanged rather than newly wrong.

Pinned by `t/raw-invocant-arrives-as-container.t` (26 tests, byte-identical
output under `mutsu` and `raku`).
