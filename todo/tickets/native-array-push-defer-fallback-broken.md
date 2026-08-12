# nextsame/callsame from an overridden `push` on an `is Array` subclass does not reach the real native push

Found by the ADR-0019 E9-pre raku verification campaign (2026-08-12, Rakudo v2026.06). This is
scenario (d) of the campaign — the `native_array_storage_next_candidate` synthesized fallback
(`src/runtime/builtins_dispatch_next.rs`) is the code under test, and it does not perform the
push.

## Divergence

```raku
class MyArr is Array {
    method push(|c) { say "MyArr::push"; nextsame }
}
my $a = MyArr.new;
$a.push(1);
$a.push(2, 3);
say $a.elems;   # raku: 3   mutsu: 0  (array stays empty; no error)
```

`callsame` variant additionally gets the wrong return value:

```raku
class MyArr2 is Array {
    method push(|c) { my $r = callsame; $r }
}
my $b = MyArr2.new;
$b.push(10);
# raku:  $b[0] == 10, push returns self (=== $b is True, .^name is MyArr2)
# mutsu: $b stays empty, callsame returns Any, === $b is False
```

So the fallback neither mutates the underlying array storage nor returns the invocant. In raku
the deferral reaches the real `Array.push`, which appends and returns self.

## Where to look

`native_array_storage_next_candidate` is force-pushed with empty `remaining` when the user MRO
is exhausted (`builtins_dispatch_next.rs:181-310` per the E8-E11 design survey). Its handler
must (a) route to the same mutable native push the ordinary `$a.push(...)` dispatch uses
(`&mut self` slow path via `call_method_mut_with_values` / the `CallMethodMut` family — note the
E6c precedent: sigil-only routing there ignored overrides, same neighborhood), and (b) return
the invocant. Verify the invocant reaching the fallback is the ContainerRef/box for the actual
array so the mutation is visible to the caller's variable, not a detached copy — the observed
"no error, no effect" smells like mutating a clone.

Also worth covering when fixed: `pop`/`shift`/`unshift`/`append` through the same fallback, and
ADR-0019 E9's cursor migration (design decision 2 makes the four synthesized fallbacks ordinary
sequence tail entries — this bug should be fixed or explicitly re-tested at that boundary).

The E9-pre pin for this lands with the fix.
