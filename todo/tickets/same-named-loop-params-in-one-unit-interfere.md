# A `for` loop's multi-parameter bind is treated as a mutation of a same-named lexical

*(Retitled and rewritten 2026-09-04 after a full investigation. The original
title — "Two `for` loops in one unit that name their parameters the same
interfere" — described one symptom of a broader defect, and its central
hypothesis was disproved. The original text is preserved in the "What the
original ticket said, and what was wrong with it" section below, so nothing is
lost.)*

## The divergence

A `for` loop's **multi**-parameter list binds through an assignment. When a
parameter's name collides with **any** local slot of the same compiled unit, that
assignment writes the outer lexical's slot, and every closure created in the body
ends up sharing one container — so they all observe the last iteration:

```raku
my $x = 1;                      # an enclosing `my`, or a popped sibling block's
my @a = 10, 20, 30, 40;
my @c;
for @a -> $x, $y { @c.push(-> { $x }) }
say @c>>.();     # raku: [10 30]   mutsu: [30 30]
```

Silent: exit 0, the enclosing `$x` still reads `1` afterwards, only the closures
are wrong.

## What was established (measured 2026-09-04, debug build at `ea4cfdb57`)

1. **`MUTSU_SHADOW_SLOTS` does not fix it, and structurally cannot.** The
   original ticket's proposed first experiment was to turn the gate on. Done: the
   repro is byte-identical with the gate on and off. §1.4 mints a fresh slot only
   for a genuine **ancestor** shadow, and deliberately lets an already-popped
   **sibling** reuse its slot — and that restriction is itself a fix
   (`docs/lexical-scope-slot-campaign.md`, "Root-cause fix: shadow ⟺
   active-ancestor, not `local_map` presence"), because minting duplicate
   `code.locals` entries corrupted every by-name writeback resolver. So this is
   **not** a datapoint for §1.4.
2. **`is rw` is irrelevant.** Any same-named local reproduces it. An ordinary
   enclosing `my $x` (very common) is enough; so is `{ my $x = 5; }` in an
   already-closed sibling block. Two loops are not needed at all.
3. **Arity is what is load-bearing.** The single-parameter loop
   (`for @a -> $x { }`) is correct even with a colliding `my $x`, because its
   bind happens inside the `ForLoop` opcode exec and compiles to **no name-write
   op at all**. Only the multi-parameter path emits one
   (`Compiler::build_for_bind_stmts` → `Stmt::Assign` → `SetLocal`/`SetGlobal`).
4. **The chain is:** that `SetLocal` makes `CompiledCode::compute_free_vars`
   record the name in `self_mutated` → the outer local is classed
   `captured_mutated_locals` → `box_captured_lexicals` boxes it into a shared
   `ContainerRef` cell at closure-capture time → each iteration's bind then
   writes *through* that one cell, which every closure has captured.

## Two candidate fixes were implemented and both fell short

Both are recorded because each got partway and each failed for an instructive
reason. Neither was landed.

### (A) Bind with a declaration instead of an assignment

The `@`/`%` arm of that same match in `build_for_bind_stmts` **already**
declares, with the reason written out ("a fresh per-iteration lexical, not an
alias of a same-named outer"), so bringing the plain-scalar arm in line looks
like the obvious fix — and it does fix every standalone shape above.

It breaks **nested same-named multi-param loops**
(`t/for-multi-param-shared-lane.t` test 4,
`t/for-multi-param-type-constraint.t` test 10): the VM's save/restore of the
shadowed outer binding (`vm_for_loop_body.rs`, `saved_multi_params`) is keyed on
`ForLoopSpec::multi_param_locals`, a snapshot of `local_map` taken **before** the
bind prefix compiles. A declaration can allocate a slot that snapshot does not
know about, so the slot is never restored and the inner loop's values leak out.
Patching the spec after the bind prefix compiles (the `loop_idx` op is already at
hand) changes the failure rather than removing it — the outer loop then restores
`Nil`. The whole `saved_multi_params` apparatus is written around the assumption
"a multi-param binds via plain `Stmt::Assign`", and untangling it is the real
size of this work.

### (B) Exclude the bind ops from the mutation scan, by op POSITION

Record which op positions the readonly plain-scalar multi-param binds compile to
(`for_loop_readonly_multi_param_bind_ops`) and skip exactly those in
`compute_free_vars` — in both the name-write branch (`op_name_write_const_idx`,
which catches the `SetGlobal` spelling) and the slot-based `OpCode::SetLocal`
arm.

Positions, not names: a name set also swallows a *genuine* mutation of the
enclosing lexical elsewhere in the unit, which was verified to break three real
shapes (`my $x = 1; my $c = -> { $x }; $x = 2; for @a -> $x, $y {}` must still
let `$c()` see `2`). The position-keyed version keeps all three correct.

This fixes the original ticket's repro and every standalone shape, and breaks no
existing test. It is **still not enough**: as soon as the same-named outer
lexical *is* genuinely mutated somewhere in the unit, it legitimately earns its
cell, and the loop's bind writes through it again. Severing the cell per
iteration at the bind op (the loop setup already severs once at loop *entry*)
does not help either — the closure does not take its value from the slot at that
point. That residue is enough to poison a whole `t/` file: any file containing
one genuinely-mutated `$x` makes every multi-param `$x` loop in it wrong again,
which is why the partial fix could not be honestly pinned by a test and was not
landed.

## Where this really sits

The residue is the shared local slot: two `$x` bindings in one unit are one slot,
so no per-binding cell decision is expressible. That is §1.3 (slot-indexed
locals / retiring the by-name resolvers) of
`docs/lexical-scope-slot-campaign.md` — **not** §1.4, and not a heuristic that
can be added around the edges. Fix (B) is a correct, precisely-scoped
*component* of that work and is worth re-implementing on top of it; fix (A)
additionally needs `saved_multi_params` re-keyed off something other than a
pre-bind `local_map` snapshot.

## What the original ticket said, and what was wrong with it

It framed the bug as two loops interfering — an `is rw` loop earlier in the unit
changing what a later, unrelated, non-rw loop's closures capture — and noted the
interference ran both ways. That is a real symptom, but it is a special case:
`is rw` is not required, a second loop is not required, and the "likely cause"
section correctly named `captured_mutated_locals` / `needs_cell_locals` while
attributing the fix to the wrong campaign phase (§1.4's shadow-slot gate, which
was measured to make no difference).

## Re-verified 2026-09-05

Still reproduces byte-for-byte on `main` at `e4994a3`: the four-line repro prints
`[30 30]` where raku prints `[10 30]`. Nothing in this file has drifted — the two
rejected candidate fixes and the §1.3 placement above stand as written, and the
work still needs its own session on the slot campaign rather than a slice
alongside unrelated tickets.

## Reproduce

The four-line repro at the top, no fixtures. Two more that isolate the residues:

```raku
# residue: a genuine outer mutation re-earns the cell
my @a = 1, 2, 3, 4;
my $x = 1;
my $c = -> { $x };
$x = 2;                       # this write is real and must keep its cell
for @a -> $x, $y { }
say $c();                     # raku 2 -- correct today, and fix (B) keeps it

# the single-parameter form is correct today and must stay correct
my $z = 1;
my @b = 10, 20, 30; my @cs;
for @b -> $z { @cs.push(-> { $z }) }
say @cs>>.();                 # raku [10 20 30], mutsu [10 20 30]
```
