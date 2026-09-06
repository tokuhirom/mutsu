# A `:=` of a free variable declared in a BARE BLOCK still aliases an intervening caller's lexical

> This file is the **residue** of
> `todo/deep/free-var-read-in-callee-resolves-through-dynamic-caller-chain.md`.
> The file-scope half of that finding was fixed on 2026-09-07 — see
> `news/2026-09/free-var-bind-aliased-caller-lexical.md` — and its oracle,
> `t/free-var-bind-does-not-alias-caller-lexical.t`, is now 22/22 green under
> mutsu and raku with no `todo`. Everything below is what is *left*, re-measured
> on the fixed build. Do not re-derive the file-scope matrix; the news entry has
> it.

## The repro

```raku
{
    my $bv = 1;
    my $ba;
    sub gb() { $ba := $bv }
    sub fb() { my $bv = 5; gb(); $bv }
    say fb();          # raku: 5     mutsu: 1
    $bv = 9;
    say $ba;           # 9 in both
    say $bv;           # 9 in both
}
```

The identical shape at file scope prints `5` (rows H / O of the pinned test).
The `bind-then-statement` spelling of the same thing inside a block
(`sub gc2() { $ca := $cv; 0 }` with the caller writing `$cv = 7` afterwards)
already answers as raku, so only the last-statement (`SetGlobal`) spelling is
known to diverge in the block surface — but treat that as "not yet measured
exhaustively", not as a boundary.

## Why the file-scope fix does not cover it

The 2026-09-07 fix works because a compunit / mainline file-scope `my` captured
by a named sub has a **lexical answer available at run time**: ADR-0024's
`unit_lexicals[MAINLINE_UNIT_KEY]` cell. `Interpreter::unit_scope_lexical_bind`
(`src/vm/vm_env_helpers.rs`) rebinds *there* and both bind handlers then skip the
two by-name routes that would otherwise deposit the cell in an intervening
caller's env tier.

A `my` declared in a **bare block** is in no such store — `exec_register_sub_op`
gates the ADR-0024 capture on `block_scope_depth() == 0` — so
`unit_scope_lexical_bind` returns `false` and both routes run:

1. `self.env_mut().insert(resolved_source, container)` in the callee's overlay,
   which `call_compiled_function_named_inner`'s return merge
   (`vm_call_named_inner.rs`, the `restored_env.insert_sym` loop) copies into the
   caller's own tier for every name that is not a callee local;
2. `propagate_bind_to_ancestor_frames` (`vm_var_assign_ops.rs`), whose
   `saved_env.contains_key_own_tier(name)` gate matches the caller's
   **redeclaration `Nil` marker** — the entry `exec_set_local_op_inner`'s guard
   writes to mean "this is a FRESH binding, do not inherit the outer cell".

Both were confirmed on the fixed build with a `rust-gdb` breakpoint on
`Env::insert_sym` conditioned on the name's interned `Symbol` id (get the id by
breaking on the adopt at `src/vm/vm_var_assign_local_get.rs:273` and printing
`code.locals_sym`). The caller then adopts the cell at that same adopt line.

## Why this is the env-model change, not another guard

The discriminator the two routes need is *which declaration the callee's free
variable resolved to*. Both the declaring block frame and the shadowing caller
frame `my`-declare the same name, and both hold it in a local slot, so no
name-keyed signal available at the bind site can tell them apart:

- `code.my_declared_sym` is true for both frames.
- "this frame declared this slot in this invocation" is true for both frames.
- `overlay_get`'s tier restriction (the previous narrowing) does not help: the
  merge deposits the cell in the caller's *own* tier, not an ancestor's.

Answering it requires a routine's env parent to be its **lexical scope** rather
than its caller — exactly the question ADR-0055 §7.5 records as out of scope, and
the thing that wants its own ADR. The ADR-0024 store is that answer, precomputed,
for the one scope where it exists; the general form is the campaign.

## If you want a smaller step first

Extending the ADR-0024 capture to block-scope named subs (lifting the
`block_scope_depth() == 0` gate in `exec_register_sub_op`, with a per-block
bucket instead of the single `MAINLINE_UNIT_KEY`) would close this repro through
the same door the file-scope one went through, without an env-model change. That
is a real design decision with its own blast radius (the store is consulted
before `env` on every by-name read via `get_env_with_main_alias`), so measure the
cost of a second bucket before committing to it — but it is a strictly smaller
piece of work than lexical env parenting, and it would tell you how much of the
remaining surface the store shape can actually cover.

## Pin

`t/free-var-bind-does-not-alias-caller-lexical.t` is deliberately flat (file
scope). Do not add block-scope rows to it while they diverge — add them in the
same file, un-`todo`'d, as part of closing this.
