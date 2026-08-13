# nextsame/callsame from an overridden push/pop/shift/unshift/append on an `is Array` subclass now works

Found by the ADR-0019 E9-pre raku verification campaign (2026-08-12): `nextsame`/`callsame` called
from a user-overridden array-mutator method on a class `is Array` silently did nothing — the array
stayed empty, with no error.

```raku
class MyArr is Array {
    method push(|c) { say "MyArr::push"; nextsame }
}
my $a = MyArr.new;
$a.push(1);
$a.push(2, 3);
say $a.elems;   # raku: 3   mutsu (before this fix): 0
```

`callsame` additionally returned the wrong value: raku's base `Array.push` returns the invocant
itself, so `my $r = callsame` inside an overridden `push` should hand back `self` (same identity,
same subclass type), but mutsu returned `Any`.

## Root cause

The synthesized fallback used when `nextsame`/`callsame` exhausts the user-defined MRO on an
`is Array` subclass — `native_array_storage_next_candidate` in
`src/runtime/builtins_dispatch_next.rs` — had two independent bugs:

1. **Wrong dispatch target.** It routed every method through `try_native_method`, which is the
   *pure*, non-mutating native dispatch (`&Value` receiver, no `&mut Interpreter`). That dispatcher
   has no entry at all for `push`/`append`/`prepend`/`unshift`/`pop`/`shift` — the same
   sigil/routing-blind pattern that has bitten this codebase before (the E6c precedent) — so it
   silently returned `None` for every one of them, and the fallback gave up with no error.

2. **Missing call args in the common case.** A single, non-multi, non-wrapped method override —
   exactly the shape of `method push(|c) { nextsame }` — pushes no `method_dispatch_stack` frame at
   all (there is no MRO ambiguity to defer through). The fallback's arg recovery only ever looked
   at `method_dispatch_stack`, so even once dispatch bug #1 was fixed, the mutation would have run
   with an empty argument list.

## The fix

`native_array_storage_mut` (`src/vm/vm_call_method_mut_ops.rs`) — the same helper the direct
`$a.push(...)` fast path already uses to mutate an `is Array` instance's backing
`__mutsu_array_storage` — was promoted to `pub(crate)` and reused by the fallback instead of
`try_native_method`. It runs under `InstanceAttrs::with_attr_mut`, which hands out a `&mut Value`
into the instance's *shared* attribute cell, so the mutation is visible to every other holder of
the same instance rather than a detached copy. `push`/`append`/`prepend`/`unshift` now map their
result to the invocant itself (matching raku's `self`-returning semantics), while `pop`/`shift`
keep returning the removed element.

For the missing-args case, a new `samewith_call_args_stack: Vec<Vec<Value>>` field is pushed and
popped in lockstep with `samewith_context_stack` by `push_method_samewith_context`/
`pop_method_samewith_context` (`src/runtime/accessors_state.rs`) — giving the fallback a place to
recover the original call args on a single-candidate method dispatch, which previously had no
carrier at all. The stack is GC-rooted (`src/runtime/gc_roots.rs`) like every other live-args
container in the interpreter.

## Verification

New pin: `t/native-array-push-defer-fallback.t` (16 assertions), independently verified against
Rakudo v2026.06 before being run against mutsu — covers `nextsame` push (single and multi-element),
`callsame` push return-value identity and subclass type, `pop`/`shift`/`unshift`/`append` through
the same fallback, `nextwith`'s explicit-args override still winning over the recovered original
args, and a sanity check that a plain (non-subclassed) `Array.push` is unaffected. Local roast
slice (`S06-advanced/{callsame,dispatching}.t`, `S02-types/array.t`, `S12-attributes/instance.t`,
`S12-methods/instance.t`) and the existing `t/array-subclass-vector.t` /
`t/array-subclass-new-default-ctor.t` regression pins all remain green.

## Follow-up filed separately

The direct (non-deferred) `$x.push(1)` call on a plain `is Array`-backed instance with *no*
override has the same return-identity bug — it returns the raw backing array instead of the
invocant — in the unrelated fast path in `vm_call_method_mut_ops.rs`. That was out of scope for
this fix and is tracked in
`todo/tickets/array-subclass-push-returns-storage-not-self.md`.

This closes one of the two divergence tickets left open by the ADR-0019 E9-pre campaign; only
`explicit-child-proto-assumes-parent-candidates.md` remains.
