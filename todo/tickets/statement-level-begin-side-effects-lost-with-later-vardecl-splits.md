# A statement-level `BEGIN {...}` block's side effects on an outer array/hash are lost when the block also contains a later split VarDecl

Found while writing a regression test for
`todo/tickets/constant-declared-from-a-begin-is-rejected.md`. Pre-existing on
`main`; not a regression from that fix (reproduces identically with `git
stash` applied against the unpatched tree, and with a plain `my`, not just
`constant`).

## Repro

```raku
my $z = BEGIN 99;    # any VarDecl with a nested BEGIN/CHECK/INIT PhaserExpr
                      # is enough to trigger reordering for the whole block
my @order;
BEGIN { @order.push('begin') }
my $i = 20;
@order.push('after');
say @order;          # raku: [begin after]   mutsu: [after]
```

The first `push('begin')` is silently lost. Any `my`/`state`/`constant`
VarDecl earlier in the same statement list that itself has a nested
BEGIN/CHECK/INIT phaser expression is enough to trigger this (via
`has_other_phasers` in `reorder_at_level`, `src/runtime/phasers.rs`) — a
`constant`-specific repro is not required.

## Where to look

`reorder_at_level` (`src/runtime/phasers.rs`) buckets the block's statements
into `var_decls` / `use_stmts` / `begin` / `check` / `init` / `rest` and
reconstructs them in that fixed order. The statement-level `BEGIN {
@order.push('begin') }` lands in the `begin` bucket (hoisted ahead of
`rest`), and `my @order;` (a bare, no-init VarDecl) lands in `var_decls`
(also ahead of `begin`). Reconstruction order is `var_decls, use, begin,
extra_begin, check, extra_check, init, extra_init, rest` — so on paper `my
@order;` runs, then the `BEGIN` push runs, then `rest` (the later `my $i =
20;`'s split-out assign, and `@order.push('after')`). That ordering looks
right; the actual value getting lost suggests either:

- the `BEGIN` phaser's `@order.push('begin')` is running against a
  *different* `@order` binding than the one `rest`'s `@order.push('after')`
  later writes to (a container-identity mismatch, maybe from how the
  bucketed statements are recompiled into a different code-object/block
  scope), or
- `@order` gets re-initialized to empty somewhere between the BEGIN bucket
  and `rest` running.

Needs a bytecode dump (`--dump-ast` won't show the post-`reorder_phasers`
tree — check whether there's a way to inspect it, or add a temporary debug
print in `reorder_at_level`) and/or `rust-gdb` breakpoints on the array's
push sites to see which container each push actually lands in.

## Scope

Not required by `todo/tickets/constant-declared-from-a-begin-is-rejected.md`
(that ticket's own repro cases do not exercise this — they have no
statement-level `BEGIN {}` mixed with array mutation). Filed separately so
it isn't lost; low urgency (no known roast/Cro blocker), but it is silent
data loss for ordinary Raku code, so worth fixing eventually.
