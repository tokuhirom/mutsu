# A multi-dim assignment through a subscript chain reaches the real container

`%o<inner>{1;2} = 5` used to leave `%o` empty. The write was dropped with
nothing reported — a Tier S silent-data-loss row in `todo/TRIAGE.md`.

## Root cause

When the target of a multi-dim assignment is not a plain variable, the compiler
fell back to `OpCode::MultiDimIndexAssignGeneric`, which pops the target
*value* off the stack and mutates that. The ticket recorded this as "mutates a
throwaway copy", but the measured behaviour was narrower and explains why the
gap had gone unnoticed for so long: when the chain named a container that
already existed, the popped value shared its `Gc` backing store with the
original, so the write *did* land —

```raku
my %o; %o<i> = []; %o<i>[0;1] = 5;   # worked: {:i($[[Any, 5],])}
```

Only an **autovivified** level was lost, because installing a fresh container
into the parent needs a reference to the parent, and the generic op does not
have one. That is the overwhelmingly common case (`%o<inner>` normally does not
exist yet), which is why the headline repro looked like a total loss.

## The fix

A multi-dim target that is a subscript chain rooted at a named variable is no
longer compiled to the generic op. `Compiler::index_chain_target` decomposes
`%o<inner>{1;2}` into the root name `%o`, the chain prefix `[<inner>]` (with the
`is_positional` flag of each prefix subscript), and the `{1;2}` dimension group,
and emits the new `OpCode::MultiDimIndexAssignNested`. The VM resolves the root
exactly as the named op does — through the shared `ContainerRef` cell when the
variable was captured by a closure, otherwise the env/locals pair — then walks
the prefix, autovivifying each missing level with the bracket kind of the
subscript that follows it (`%o<a><b>` makes a Hash, `%o<a>[0]` an Array), and
hands the dimension group to the existing `multi_dim_assign`. A level this walk
had to create is itemized on the way back out, the same rule
`fresh_autoviv_container` applies to the single-subscript chain, so
`%o<inner>{1;2} = 5` renders `{:inner(${"1" => ${"2" => 5}})}` like rakudo.

The root-resolution and write-back dance the named op performed inline is now
`Interpreter::mutate_named_container`, shared by both ops. It grew one arm the
old code lacked: a variable present only in the locals slot is mutated there
instead of having its write silently discarded.

## The positional spelling is a refusal, not a write

`%o<inner>[0;1] = 5` is not merely unimplemented — rakudo rejects it:
`ASSIGN-POS` has no candidate taking more than one index on an undefined
invocant, so it throws `X::Multi::NoMatch`. mutsu used to drop that write
silently through the generic op, and (in the named path, `my $x; $x[0;1] = 5`)
happily autovivified where rakudo throws. `multi_dim_assign` now refuses a
positional multi-dim subscript on an undefined container in both paths, with
rakudo's `Cannot resolve caller ASSIGN-POS(Any:U: Int:D, Int:D, Int:D)` capture.
The associative spelling still autovivifies — `ASSIGN-KEY` *is* defined on
`Any:U`, which is why `my $x; $x{1;2} = 5` builds the nested Hash chain.

Because the refusal happens mid-descent, the chain walk rolls back every level
it had autovivified on the way down (a created hash key is removed, a grown
array is truncated), so a refused assignment leaves the container untouched —
byte-for-byte what rakudo leaves.

Pin: `t/multidim-assign-expression-target.t` (20 assertions, all cross-checked
against `raku`).
