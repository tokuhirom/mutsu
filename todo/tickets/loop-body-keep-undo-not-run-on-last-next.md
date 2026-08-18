# `KEEP`/`UNDO` phasers inside a loop body never run when the iteration exits via `last`/`next`

Found while fixing
`todo/tickets/keep-undo-decided-by-value-truthiness-not-completion.md`
(the trailing-value-truthiness-vs-definedness bug) and writing its
regression test: this is a separate, pre-existing gap in the same area,
confirmed to reproduce identically on `main` before that fix (via `git
stash`), so it is not a regression introduced by it.

## Repro

```raku
my $s = "";
for 1 { KEEP { $s ~= "K" }; UNDO { $s ~= "U" }; last }
say "[$s]";
$s = "";
for 1,2 { KEEP { $s ~= "K" }; UNDO { $s ~= "U" }; next }
say "[$s]";
```

```
raku:  [U]
       [UU]
mutsu: []
       []
```

Real Raku runs `UNDO` (not `KEEP`) when a loop iteration is interrupted by
`last`/`next` before falling off the end of the block — see the definedness
rule documented in `should_run_success_queue`
(`src/vm/vm_misc_block.rs`)/`should_run_success_queue_raw`/`_vm`
(`src/runtime/run.rs`): a `last`/`next` exit's `return_value` is `None`
(reads as undefined `Nil`), so it should route to the failure/UNDO queue
exactly like an undefined trailing value would. mutsu instead runs
**neither** queue for a loop body interrupted this way — the KEEP/UNDO
dispatch (`should_run_success_queue*`) is apparently never reached at all
for a loop-body block on a `last`/`next` exit, not merely mis-decided.

## Why this is a separate ticket

The truthiness-vs-definedness ticket's fix only changes the DECISION logic
inside `should_run_success_queue*`; those functions are simply never called
(or their result is discarded) for a loop-body block that exits via
`last`/`next`. Root-causing that requires tracing how a `for`/`while`/`loop`
body's own LEAVE-family phaser dispatch differs from a plain bare block's
(`run_block`/`exec_block_scope_op`) — not yet investigated.

## Severity

Low: narrow (`last`/`next` are unusual inside a block declaring `KEEP`/`UNDO`
phasers — the ordinary use is a bare `{ ... }` scope or a routine body, both
of which are already correct), and no roast test currently depends on this.
