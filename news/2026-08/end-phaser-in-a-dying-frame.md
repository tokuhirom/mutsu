# An END phaser registered inside a call sees that frame's final lexicals

```raku
sub f { my $x = 5; END { say "x=$x" }; $x = 7 }
f();
```

Rakudo prints `x=7`; mutsu printed `x=5`. The same held for an anonymous sub, a
pointy block and a method body.

This finishes `news/2026-08/end-phaser-sees-live-lexicals.md`. That change made
an END phaser read the *live* value of any lexical whose scope is still alive,
and freeze the captured copy of any name whose declaring scope died — the
captured copy being the last surviving binding of that name. But it only wired
the freeze into one scope-death site, the block-scope exit in
`src/vm/vm_misc_scope.rs`, which hands `update_end_phaser_envs` the block's
`block_declared` set. A *call* frame dies elsewhere, and none of those exits
refreshed the capture, so a phaser registered during a call kept whatever the
frame held at the instant it was registered.

## The four call paths

The same three lines go into each, all gated on `end_phaser_count()` having
grown during the call, so a program with no END phaser pays one integer compare
per call:

| path | file | how `dying` is computed |
| --- | --- | --- |
| named sub | `vm_call_named_inner.rs` | callee env keys absent from the post-merge `restored_env` |
| closure / anonymous sub / pointy block | `vm_closure_dispatch.rs` | same, against its `restored_env` |
| method (merge path) | `vm_method_dispatch.rs` | callee env keys absent from `merged_env` |
| method (fast path) | `vm_method_dispatch.rs` | callee env keys absent from the restored caller env |

The rule is the same everywhere and needs no new bookkeeping: **a name the
callee env holds that the caller env does not is one this frame takes with it**.
Those get frozen; everything else stays unfrozen and keeps reading live, so a
later mutation of a caller variable the body touched is still what the phaser
sees.

The fast method path needed one extra step. A `can_skip_merge` method keeps its
`my` variables in local slots and never writes them to env at all, so
snapshotting the env would have captured nothing and left the registration-time
copy in place. The snapshot therefore runs `sync_env_from_locals` first, pushing
the slots into the callee env that is about to be discarded anyway.

The closure path already had a *complementary* refresh —
`update_end_phaser_envs_for_keys` over the closure's captured names, for an
outer variable the body mutated. That one stays: it covers names that remain
live in the caller, which is exactly the set the new freeze leaves alone.

## Tests

`t/end-phaser-live-lexical.t` grows from five cases to nine — sub, anonymous
sub, pointy block and method added to the module / mainline / dead-block /
repeated-sub / LIFO cases already there. All nine produce identical output under
`raku`.

Local `make test` and `make roast` both pass.
