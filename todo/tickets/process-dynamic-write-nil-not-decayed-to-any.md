# `PROCESS::<$x> = Nil` stores a literal Nil instead of decaying to Any

Found while fixing `todo/tickets/leave-phaser-process-write-lost-in-loop-body.md`
(a block-scope-exit dynamic-var write propagation bug) and writing its
regression test — a separate, narrower, pre-existing bug in the same code
path, unrelated to the propagation fix.

## Repro

```raku
PROCESS::<$X> = 42;
PROCESS::<$X> = Nil;
say PROCESS::<$X>.^name;   # raku: Any   mutsu: Nil
say PROCESS::<$X>.WHAT;    # raku: (Any) mutsu: Nil
```

No block or loop involved — reproduces on a completely flat, unnested
assignment.

## Root cause (not yet fixed)

`store_process_dynamic` (`src/vm/vm_var_assign_index_named.rs`) stores the
value via `Self::itemize_scalar_store(&env_key,
Self::normalize_scalar_assignment_value(val))`.
`normalize_scalar_assignment_value` (`src/vm/vm_var_assign_typed.rs`) only
collapses a single-nilish `Seq`/`Slip` down to `Nil` — it does not decay a
bare `Nil` value into the `Any` type object the way an ordinary `my $x = Nil`
scalar assignment does elsewhere in the VM. Whatever opcode/helper performs
that decay for a plain lexical scalar assignment is not on this
`PROCESS::`/pseudo-stash write path.

## Severity

Low: narrow (only affects assigning a literal `Nil` specifically through
`PROCESS::<$name> = ...`, not general dynamic-var writes with any other
value), and no roast test currently depends on this. Worth fixing for
correctness/consistency but not blocking anything.
