# A module's file-scope `my @a` / `my %h` is still the caller's variable

**Design: [ADR-0039](../../docs/adr/0039-container-lexicals-resolve-lexically.md).**

- **Slice 1 (§4.1) LANDED 2026-08-20.** The module shape, the module-free
  mainline shadow shape, and the sub-local-consumer shape from this file's
  original repros are fixed and pinned (`t/module-file-scope-lexical.t`,
  `t/named-sub-lexical-scope-container.t`).
- **Slice 2 (§4.2) re-measured and re-withdrawn 2026-09-06.** See ADR-0039 §10
  for the enumeration. The short version is below, because it changes what
  "remaining scope" means for this file.

## The 2026-09-06 re-measurement — read this before planning more work

Every row of ADR-0039's matrices, plus **one row per entry of slice 1's
exclusion list** (`our`, `state`, `is export`, `$*dynamic`, `::`-qualified,
type-constrained, anonymous container) in both the module file-scope shape and
the mainline named-sub shadow shape, was run under `raku` v2026.07 and debug
`mutsu` on `2a9e06f91`. Full table: ADR-0039 §10.1.

> **The exclusion list is not a list of divergences.** Every one of those rows
> already agreed with `raku`. ADR-0039 §4.3's "slice 1's exclusion list is the
> list of things slice 2 must subsume" is stale — those names were excluded from
> slice 1's *store*, and the divergences they used to carry were closed
> separately (the `our` container resolution fix 2026-08-23, its scalar twin
> 2026-08-25, and §8.6's lane lifetime 2026-08-22).

That matrix is now pinned as `t/container-lexical-declarator-matrix.t`
(41 assertions, fixture `t/lib/ContainerSlotLexical.rakumod`, byte-identical
under `raku`) so the claim cannot rot back into a to-do item.

One exclusion row does still diverge, and it is a **scalar** lane bug: a
module's `my $anon = [...]` colliding with a consumer's `my $anon` (the binding
is `$`-sigiled; only its value is an Array). Split out to
`todo/tickets/module-scalar-held-array-collides-with-caller-my.md`.

## What this file's remaining scope actually is

Slice 2's read side (`@`/`%` reads compile to `GetLocal(slot)`) was implemented
and measured a second time. `prove t/` goes fully green under it once four
store-side defects are fixed (ADR-0039 §10.2 lists them precisely), and §6's
acceptance row (b) is fixed by it. But `make roast` then fails two whitelisted
files that are a different lane entirely:

- `roast/S15-nfg/concat-stable.t` — `my @n = @o.shift xx $_`, where the `xx`
  LHS is an anon-sub thunk that mutates `@o`;
- `roast/integration/advent2014-day05.t` — `$supply.act: { @seen[$_] //= $_ }`.

Both are one root cause, and it is the same as ADR-0039 §6 acceptance row (a):

> **A container mutated from a nested frame propagates to its owner by NAME
> only.** The mutating frame has no slot for the name, so the write lands in
> `env`; when the write path *replaces* the container rather than mutating it in
> place, the owner's slot goes stale. Today's by-name read hides this entirely,
> which is why the read flip cannot land without it.

The obvious repair — cell-box the container at its declaration — is already
recorded as tried and rejected: `docs/captured-outer-cell-sharing.md` §7.1d
(broader `@`/`%` decl-site boxing regressed ~12 files through decont leaks), and
that is why `register_container_ref_capture_if_free` (`compiler/expr_call.rs`)
is restricted to plain `$` names. Every "scalars only (containers share via Arc
already)" comment in `vm/vm_env_helpers.rs`'s boxing helpers rests on a premise
that is true for in-place mutation (ADR-0039 §2) and false the moment a path
replaces the container.

So this file is now **one ticket: make a container mutated from a nested frame
reach its owner's binding**, without decl-site boxing. It is ADR-0039 §4.2's
SECOND bullet, adjacent to ADR-0055's closure free-variable work, and it gates
the read flip rather than following it. Acceptance:

```raku
# ADR-0039 §6 row (a)
sub f {
    my @a = 1, 2;
    my $push = sub { @a.push(9) };
    { my @a = 3; $push(); say "inner=", @a }   # raku: inner=[3]   mutsu: inner=[3 9]
    @a
}
say f();                                       # raku: [1 2 9]     mutsu: [1 2]
```

plus the two roast files above (both already whitelisted, so a regression is
loud).

## What shipped 2026-09-06

Only the part correct and pinnable without the read flip:
`Interpreter::store_container_preserving_identity` and its three call sites (the
`map` builtin's rw writeback, the `.map` method's rw writeback,
`classify`/`categorize`'s `:into`), which write a rebuilt container's contents
through the existing backing node. Two real bugs fixed —
`my $b := @a; map { $_ = 5 }, @a` left `$b` at `[1 2 3]`, and
`my $f := %into; @src.categorize(..., :into(%into))` left `$f` empty. Pin:
`t/container-rebuild-preserves-identity.t`.
