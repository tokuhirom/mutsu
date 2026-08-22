# A multi-parameter `for` loop no longer publishes its per-iteration binding cross-thread

A `for @xs.kv -> $i, $comp { ... }` loop was rewriting the `$i` of a completely
unrelated frame — in another file, in another routine — once any thread had
been spawned in the process.

`build_for_bind_stmts` binds a multi-parameter loop's variables with a plain
`Stmt::Assign` rather than a `my`-style declaration. That assignment reaches
`set_shared_var_sym`, which (while the cross-thread store is active) publishes
the value under the variable's **bare name** into the process's lineage store.
Every subsequent `await` then runs `sync_shared_vars_to_env`, which pulls the
dirty bare-name entry back into the awaiting frame's `env` *and* — via
`pending_caller_var_writeback` — into that frame's local slot. A
single-parameter loop was never affected: it binds natively through the
`ForLoop` opcode and never touches the name lane.

The result, with no threading anywhere in the user's own code:

```raku
sub compose(@components) {
    my $last;
    for @components.kv -> $i, $comp { $last = $i + $comp }
}
await start { 1 };
for 1..5 -> $i {
    compose([10, 20, 30, 40, 50]);
    await start { 1 };
    say "loop i=$i";     # raku: 1 2 3 4 5;  mutsu: 1 4 4 4 4
}
```

A multi-param loop variable is a fresh per-iteration binding, which is exactly
the shape the bare-name store cannot represent — the same reason
`exec_set_var_dynamic_op` masks a `my` re-declaration out of the lane.
`exec_for_loop_body` now installs that mask for the loop's parameter names on
entry and drops it on every exit path, including the two type-check errors and
the two error propagations that bypass the normal restore. A `start` block that
genuinely captures such a variable is unaffected: it gets a per-binding
`ContainerRef` cell from `box_captured_lexicals`, the mechanism the name lane is
redundant with.

Found while chasing Cro's `t/http-session-inmemory.rakutest`, where
`Cro.rakumod`'s pipeline compose (`for @components-in.kv -> $i, $comp`) was
overwriting the `$i` of the test's own `for 1..5 -> $i` request loop, so all
five requests reported "Visit 4". The remaining failures on that file no longer
involve the store at all — they arrive through the `env` axis, and
`news/2026-08/shared-store-bare-name-collision-across-unrelated-frames.md`
records the re-measurement and the container-lane fix that closed it.

Pinned by `t/for-multi-param-shared-lane.t`.
