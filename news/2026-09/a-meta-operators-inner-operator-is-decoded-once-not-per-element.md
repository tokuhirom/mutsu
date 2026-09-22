# A meta-operator's inner operator is decoded once, not per element

`Interpreter::eval_reduction_operator_values` is the VM's "apply this infix to
these two values" entry point, and it took its operator as a *string*. Before
doing anything with it, it re-derived the operator's whole structure from that
string: strip a `[op]` bracket and recurse, strip an `R` reverse prefix and
recurse with the operands swapped, check for a bare `Z`, strip a `Z` and zip,
strip the `>>op<<` hyper delimiters and distribute, then fold the Unicode
aliases (`×` → `*`, `≤` → `<=`, …) before finally reaching the operator tables.

That entry point is called **per element**: once per pair of a `Z`/`X`/hyper,
once per fold step of a reduction, once per nested Hash/Pair/list level a hyper
descends into, and once per element of a lazy triangle scan's batch. So
`@a Z+ @b` over 200-element operands ran the decode 200 times and learned the
same thing each time — the operator's spelling does not change between two
elements of the same operation.

[#8997](https://github.com/tokuhirom/mutsu/issues/8997)'s first slice
([#9029](https://github.com/tokuhirom/mutsu/pull/9029)) removed the
per-*execution* decode of the statically spelled operators, the ones the
compiler owns. This is the per-*element* half, which that slice's write-up
named as the bigger prize.

## What it looks like now

`InfixShape::lower` decodes the spelling once into a stack of `MetaLayer`s
(`Reverse`, `ZipTuple`, `Zip`, `Hyper { dwim_left, dwim_right }`) around a leaf,
and `InfixRef` is the borrowed, `Copy` cursor the element loops recurse with, so
descending into a `Z+`'s inner `+` for the next element is a slice bump rather
than another parse. `Interpreter::eval_infix_shape` (new,
`src/vm/vm_infix_shape.rs`) walks the layers; `eval_infix_leaf` — the operator
tables, the numeric/stringy coercion bridges, the user-infix fallback — is the
old function's remainder and stays in `vm_dispatch_helpers.rs`.

The leaf borrows the spelling it was decoded from, since every layer strips a
prefix or a suffix. A plain operator therefore decodes to zero layers and
allocates nothing at all, and nothing is interned along the way — interning a
lookup key is exactly the per-element work this removes, which is the same trap
the first slice hit when `infix_names` was first keyed by `Symbol`.

Threaded through every site that applies an operator per element: `hyper_op_pair`
(including its Hash, Pair and nested-list recursions), the binary and n-ary `X`
and `Z` arms, the `X[+=]` / `Z[+=]` assignment forms, the reduction executor's
fold / scan / chain-comparison loops, and the lazy scan's batch loop.
`reduction_step_with_args` takes the decoded operator too, so a fold decodes
once for the whole list rather than once per step.

`[op]` records no layer: as an inner operator it is exactly `op` applied once,
which is the identity `MetaKind::Reduce` already documents. One consequence is a
unification rather than a preservation — `[=]` and `[~~]` now take the same leaf
path as `=` and `~~` in a hyper. The leaf-only `=` / `~~` special cases are
matched through `InfixRef::as_plain`, which answers only for an operator that
carries no layer at all, so a `Z=` or an `R~~` still reaches its own arm.

## Measured

A benchmark of 15,980 element applications (20 rounds of `Z+`, `>>+<<`, `[+]`
and `Zmin` over 200-element operands), both sides `--profile profiling` under
callgrind, warm second run, before = `d80a72db`:

| | before | after | |
| --- | ---: | ---: | ---: |
| program total | 41,115,963 | 40,555,310 | **-1.36%** |
| the entry point's own cost | 2,604,940 (6.34%) | 2,049,700 (5.05%) | **-21.3%** |
| `canonical_infix` | 255,867 | 985 | -99.6% |
| `strip_hyper_delimiters` | 128,000 | 0 | -100% |

That is about 35 instructions per element application. -1.36% is below what a
paired wall-clock run resolves on this box, so the claim is "strictly less work
per element", not "this is faster" — but unlike the first slice's -0.20% it
scales with the operand length rather than with the number of operations.

## What is still there

The leaf is still a string. `eval_infix_leaf` (5.05% of that benchmark) and
`apply_reduction_op` (3.26%) decide what the operator *is* by comparing its
spelling against `","`, `"minmax"`, `"^^"`, `starts_with('Z')` and so on. Turning
those builtins into enum variants — `Custom(Symbol)`'s shape is already what
every leaf has — is the second item on #8997's remaining list, and it is the
~8% this one does not touch.
