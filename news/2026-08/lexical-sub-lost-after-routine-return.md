# A lexical `sub` no longer disappears when its declaring routine returns before a captured closure runs it

A routine body that declares an inner `sub` snapshots the runtime routine
registry around the call and restores it on return, so the lexical `sub`
stops being callable by name once the declaring routine is gone — unless it
escaped via the return value (`return_value_escapes_routine`). That escape
analysis only recognized the routine's own return slot, so every
*side-channel* escape was invisible to it: a closure literal created during
the call and handed to `.tap`, stored in an attribute, pushed onto an array,
or registered as any other kind of callback. Once the declaring routine's
frame was gone, invoking that closure later could no longer resolve the inner
sub by name — `find_compiled_function` requires the runtime registry lookup
to succeed before it will use the compiled-fns table, so the bytecode still
existed but was unreachable.

```raku
sub make-closure() {
    sub helper($x) { "got $x" }
    return { helper($_) };
}
my $cb = make-closure();
$cb(42);   # mutsu (before): dies with "Unknown function: helper"
```

This broke `Cro::HTTP2::ConnectionState`'s `submethod TWEAK` pattern — a
lexical `check-window-size` sub called from a `.tap` callback registered in
`TWEAK`, invoked long after `TWEAK` returned — which in turn made every Data
frame in `Cro::HTTP2::FrameSerializer` hang waiting on a promise that never
resolved.

## Fix

Added a monotonically increasing `Interpreter::closures_created` counter,
bumped by the four closure-literal-creation exec ops (`MakeAnonSub`,
`MakeAnonSubParams`, `MakeLambda`, `MakeBlockClosure`). Each of the four
registry snapshot/restore gates (`vm_call_named.rs`, `vm_call_fast.rs`,
`vm_method_dispatch.rs` ×2) now also skips the restore when this counter
changed during the call, in addition to the existing
`return_value_escapes_routine` check. This is a runtime over-approximation
rather than static side-channel enumeration (deliberately, per CLAUDE.md's
guidance against incomplete static analysis): it also skips the restore
whenever *any* closure literal was created during a call that declares inner
routines, which can leave an unrelated inner routine registered a little
longer than strictly necessary, but it can never wrongly unregister one that
is still reachable, and a routine that declares no inner routines never pays
for the snapshot at all.

Pinned by `t/lexical-sub-escapes-via-side-channel.t`. In the vendored Cro
suite, `Cro::HTTP2` `t/http2-frame-parser.rakutest` went from 22/26 to 26/26
and `t/http2-frame-serializer.rakutest` from 37/40 to 44/44, both now fully
green.

See `todo/tickets/lexical-sub-lost-after-routine-return.md` (removed by this
change) for the original diagnosis.
