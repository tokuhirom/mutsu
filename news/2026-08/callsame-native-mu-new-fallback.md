# bare `callsame`/`nextsame` in a `new()` override now reaches the native Mu.new (bless)

```raku
class D { has $.x; method new(|c) { my $obj = callsame; $obj } }
say D.new(x => 5).x;
# raku:  5     (callsame reaches Mu.new, returns the built instance)
# mutsu (before): callsame returns Any; "No such method 'x' for invocant of type 'Any'"
```

`dispatch_next_candidate`'s "we're inside `new`, dispatch to the native Mu.new
(bless)" fallback (`src/runtime/builtins_dispatch_next.rs`) only triggered for
`nextwith`/`callwith` — never the bare `nextsame`/`callsame` forms, which
implicitly forward the original call's arguments rather than an explicit
list. Widened the condition to include `nextsame`/`callsame`, and when
`override_args` is `None` (the bare-call case), read the original call's
args off the method dispatch frame or the samewith context — mirroring how
`native_array_storage_next_candidate` resolves args for a no-frame single
compiled method.

The sibling divergence in the original ticket — `callsame` from a `gist`/
`Str`/`raku` override not reaching the native Mu-level implementation — is
NOT fixed by this change; it needs a bigger fix (compile-time detection of
callsame/nextsame usage, plumbed through the hot compiled method-dispatch
path's several exit points) and stays open as
`todo/tickets/callsame-to-native-mu-methods-nil.md`, now narrowed to just
that remaining gap with the root cause fully written up.

Regression test: `t/new-callsame-native-mu-fallback.t` (also pins the
already-working `nextwith`/`callwith` forms alongside the new
`nextsame`/`callsame` coverage).
