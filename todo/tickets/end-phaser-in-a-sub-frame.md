# An END phaser inside a sub reports its own frame's lexical at registration time

```raku
sub f { my $x = 5; END { say "x=$x" }; $x = 7 }
f();
```

Rakudo prints `x=7`; mutsu prints `x=5`.

This is the residue of `news/2026-08/end-phaser-sees-live-lexicals.md`. That fix
made an END phaser read the *live* value of any lexical whose scope is still
alive, and freeze the captured copy of any name whose declaring scope died — but
it only wired the freeze/refresh into one scope-death site: the block-scope exit
in `src/vm/vm_misc_scope.rs`, which hands `update_end_phaser_envs` the block's
`block_declared` set.

A sub frame dies elsewhere. `src/vm/vm_call_named_inner.rs` restores the caller
env at three separate exits (lines ~121, ~186, and the main path at ~436-557,
which itself forks into a `ptr_eq` fast path and a merge path), and
`src/vm/vm_closure_dispatch.rs` restores at ~1160 where it refreshes only the
closure's *captured* names (`update_end_phaser_envs_for_keys`) — a name the
closure declared itself is not among them. So a phaser registered during a call
never has its capture refreshed against the frame's final state, and the frame's
own locals are never frozen.

## What the fix looks like

Same shape as the block-scope site, applied to the sub/closure return:

1. Record `end_phaser_count()` when the frame is pushed.
2. Just before the frame's env is swapped out — one insertion point ahead of
   `let frame = self.pop_call_frame();` covers both branches of the main path —
   call `update_end_phaser_envs(count_before, self.env(), &dying)` with `dying`
   being the frame's own declared names (`cc.locals` plus the bound parameter
   names, minus anything captured from an enclosing scope).
3. Repeat for the early-exit paths at ~121 and ~186, or hoist them so they share
   one exit.

Both are guarded by `end_phaser_count() > count_before`, so a program with no
END phaser pays one integer compare per call.

## Why it was not done in the same PR

The named-sub return path is the hottest code in the VM and has several exits
with subtly different env-merge rules (`pop_caller_env` vs
`pop_caller_env_with_writeback`, rw-binding application, the `ptr_eq` fast path).
Getting the `dying` set wrong there does not fail loudly — it silently freezes a
name that is still alive, which resurrects a stale value at exit: the exact class
of bug the parent fix removed. It deserves its own PR and its own roast run.

Nothing in the vendored upstream `Test.rakumod` depends on this shape (its END
reads module-scoped lexicals, which the parent fix already covers), so it does
not block `todo/tickets/vendor-real-test-module.md`.

## Repro

```bash
printf 'sub f { my $x = 5; END { say "x=$x" }; $x = 7 }\nf();\n' > tmp/end-sub.raku
raku tmp/end-sub.raku                    # x=7
timeout 20 target/debug/mutsu tmp/end-sub.raku   # x=5
```

Case 3 of `t/end-phaser-live-lexical.t` is deliberately the *repeated-sub*
variant (a shared outer lexical), not this one; add the sub-frame case to that
file when this is fixed.
