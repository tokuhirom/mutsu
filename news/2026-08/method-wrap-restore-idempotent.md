# `.restore()`/`.unwrap()` on a method wrap: fixed the double-restore case, verified the rest already works

Filed after the ADR-0019 E9-pre raku verification campaign (2026-08-12) found:

```raku
class C { method m() { "orig" } }
my $h = C.^lookup('m').wrap(-> |c { "w-" ~ callsame });
say C.new.m;    # both: w-orig
$h.restore;
say C.new.m;    # raku: orig    mutsu: w-orig (restore silently did nothing)
```

By the time of this investigation, the `.restore()`/`.unwrap()` removal
itself (ADR-0019 E10's method-wrap-chain fix) had already landed — both the
`.restore()` and `.unwrap($handle)` repros from the ticket now match `raku`
exactly. What remained broken, found while raku-verifying the edge cases the
ticket asked for ("out-of-order removal on methods, double restore"):

```raku
my $h1 = C.^lookup('m').wrap(-> |c { "w1-" ~ callsame });
say $h1.restore;   # raku: True (both)
say $h1.restore;   # raku: False    mutsu: dies "Invalid WrapHandle: not wrapped"
```

`.restore()`'s handler treated "the wrap chain entry was already gone" as an
error for the method-candidate path (`Err("Invalid WrapHandle: not wrapped")`),
while the sub-side path always returned `Ok(Value::TRUE)` unconditionally,
regardless of whether anything was actually removed. raku's real semantics:
`.restore()` is idempotent and returns `True` the first time, `False` on
every later call — never an error.

Fixed both paths in the same `Routine::WrapHandle.restore` handler
(`src/runtime/methods_call_dispatch.rs`): the method-candidate branch now
returns `Ok(Value::truth(...))` from `remove_method_wrap`'s own bool result
instead of erroring on `false`, and the sub-side branch tracks whether the
`retain` call actually removed an entry instead of always answering `True`.
Out-of-order removal (restoring an older wrap while a newer one is still
active) was already correct and needed no change.

Extended `t/wrap-candidate-unwrap-restore.t` (10 → 15 assertions, all
byte-identical against Rakudo v2026.06) to pin the idempotent-restore
behavior for both the method-candidate and plain-sub wrap handles.
