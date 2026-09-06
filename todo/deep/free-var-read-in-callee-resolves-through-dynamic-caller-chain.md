# A `:=` bind of a free variable aliases an intervening caller's same-named lexical

> **Re-measured end-to-end on 2026-09-06 (`main`, raku v2026.07), and BOTH of this
> file's stated premises were false.** The title and the analysis below have been
> rewritten around what the measurements actually show. The oracle is now checked
> in as `t/free-var-bind-does-not-alias-caller-lexical.t` — 19 rows, all 19 green
> under `raku`, 15 green under mutsu and 4 `todo`. Do not re-derive the matrix;
> run that file.

## What is actually wrong

```raku
my $var = 1;
my $alias;
sub g() { $alias := $var; }
sub f() { my $var = 5; g(); say "f sees $var"; }
f();
```
* `raku`: `f sees 5`
* `mutsu`: `f sees 1`

and the shape of it is an **aliasing, not a stale read** — the caller's lexical
and the compunit's become *one container*:

```raku
my $n = 1; my $a;
sub gn() { $a := $n }
sub fn() { my $n = 5; gn(); $n = 7; say $n }
fn();      # 7 in both
say $n;    # raku: 1     mutsu: 7   <-- f's write reached the compunit lexical
```

It happens **during** the call: a `say $n` placed before `gn()` prints 5 and the
identical statement after it prints 1.

## Premise 1 that was false: "free-variable reads resolve through the dynamic caller chain"

They do not, today. Every one of these answers exactly as raku, with an
intervening caller shadowing the name:

| shape | result |
|---|---|
| a callee READS the free variable | callee sees the compunit's value, caller keeps its own |
| a callee WRITES it | write reaches the compunit lexical, caller keeps its own |
| **two** levels of intervening callers, each shadowing | all three frames see their own binding |
| the caller writes its own lexical, before or after the call | stays its own; compunit untouched |
| the caller declares its `my` **after** the call | unaffected |
| the bind performed in the mainline instead of a callee | unaffected, and the alias still works |
| the bind in the other direction (`$freevar := $local`) | unaffected |

Whatever the env chain does in general, it is not producing this bug, and a
campaign to make routine envs lexically parented (the ADR this file asked for)
is not what closes it.

## Premise 2 that was false: "the divergence surfaces through `propagate_bind_to_ancestor_frames`"

It does not. That function *is* reached (verified with a `rust-gdb` ignore-count
breakpoint: 1 hit) and its splice *does* fire, but **disabling the splice
entirely changes no row**. A/B'd on the same binary through an env switch, with
the splice on and off, every divergent row is byte-identical. It is a bystander.

The only other writer into an ancestor frame's `saved_env`
(`box_carrier_free_var_writes`, `vm/vm_env_helpers.rs`) is gated on
`__mutsu_in_eval` and never runs here either.

## The sharpest discriminator — start here

The two callees below differ **only in whether the bind is the last statement of
the routine**:

```raku
my $v1 = 1;
sub a1() { my $t := $v1 }        # bind is the last statement
sub f1() { my $v1 = 5; a1(); say $v1 }   # 5   -- correct

my $v2 = 1;
sub a2() { my $t := $v2; 0 }     # ANY statement after the bind
sub f2() { my $v2 = 5; a2(); say $v2 }   # 1   -- wrong
```

The target is a callee-local `my $t` in both, so this is not about the target
being free either — the earlier note that "a bind to a local target is fine" was
measured on the last-statement spelling only. Whatever leaks the source's
container into the caller runs at, or after, the *statement boundary following
the bind* — not at the bind itself, and not in the ancestor-frame splice.

Two further facts to constrain the search:

- It is **not specific to a mainline-scoped caller**: a lexical `my sub` inside a
  block shows it identically.
- The correct rows and the wrong rows behave *differently inside a bare block*
  than at file scope. The pinned test is deliberately flat for that reason; a
  probe written inside `{ }` measures a different surface and will mislead you.

### Why the two spellings differ, from `--dump-bytecode`

They compile to different opcodes for the bind itself:

```
a1 (bind is last):   GetGlobal(v); Dup; SetGlobal(t)      locals: []
a2 (statement after): GetGlobal(v); ContainerizePair;
                      WrapVarRef{name: v, slot: u32::MAX};
                      MarkBindContext; MarkVarDeclContext;
                      MarkScalarBindContext; SetLocal(0)    locals: ["t"]
```

So only `a2` runs `exec_set_local_op_inner`'s scalar-bind path
(`vm/vm_var_assign_set_local.rs`, around lines 1899-1975). `a1` never gets there
at all — its bind is a `SetGlobal`. That is the whole difference, and it means
the leak is somewhere in that bind path rather than in anything about scoping.

Narrowed further by a four-breakpoint gdb comparison of the two variants
(`unit_scope_lexical_write`, `apply_pending_rw_writeback`, `flush_local_to_env`,
`set_env_with_main_alias_inner`): the only counter that differs is
`flush_local_to_env`, 0 in `a1` and 1 in `a2` — but its backtrace shows it
flushing the bind's *target* slot (`t`), not the source, so it is a marker that
the path ran rather than the writer itself. `unit_scope_lexical_write` fires 5×
in `a1` and 3× in `a2`; `apply_pending_rw_writeback` fires twice in both.

`propagate_bind_to_ancestor_frames` at the tail of that same path is already
ruled out by the env-switch A/B above.

## Suggested next step

Locate the writer inside that bind path, don't theorise about it. The remaining
un-eliminated writers in it are `self.env_mut().insert(resolved_source, container)`
(the "update source in env" line), `set_env_with_main_alias(name, container)`
just below it, and whatever pulls a name back into the caller's local slot on
return — `f`'s `$v` lives in a compiled local slot (`locals: ["v"]`,
`SetLocalDecl { slot: 0 }`), so for `f` to read the cell, something must write
`self.locals[0]` in `f`'s frame or make `f`'s read go by name. Establishing
which of those two it is, with one breakpoint each, is the next measurement.

Only once the writer is named is it worth deciding whether this needs an ADR.
On the evidence so far it does not look like an env-model change: fifteen of the
nineteen rows, including every plain read and write at every nesting depth,
already behave correctly.

## Related

ADR-0055 §7.5 records the general "a routine's env parent is its lexical scope,
not its caller" question as out of scope for that ADR. That question is real, but
it is **no longer** what this file is about, and the two should not be conflated
again.
