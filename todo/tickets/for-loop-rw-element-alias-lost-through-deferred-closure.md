# `for @arr -> $v is rw { ... }` element aliasing doesn't survive into a closure called after the loop

## Symptom

```raku
my @list = 10, 20;
my @callbacks;
for @list -> $v is rw {
    @callbacks.push(-> { $v = $v + 1; $v });
}
say @callbacks[0]();
say @callbacks[1]();
say @list;
```

raku: `11`, `21`, `[11 21]` — the closure's `$v` still aliases the array
element even when called after the loop has finished, so the mutation
writes through.

mutsu: `11`, `21`, `[10 20]` — the callback's return values are correct (so
the closure's OWN view of `$v` is right), but the mutation never reaches
`@list`. Confirmed pre-existing via a worktree build at `af1d75401` (the
commit immediately before ADR-0027 Slice 1 / PR #6309 merged) — same broken
output there, so this is unrelated to the ADR-0027 loop-freeze-cascade
mechanism (`owned_captures`/`frame_owned`) and its Slice 1 fix.

## Where this likely lives

`for`-loop `is rw` element aliasing presumably ties the pointy param to the
source array element via some `ContainerRef`/`TagContainerRef` mechanism at
bind time (see `build_for_bind_stmts` in `src/compiler/mod.rs` and the
`has_rw`/`has_sigilless` handling around `src/compiler/stmt.rs:2170`-2230).
That aliasing evidently works for direct in-body mutation (`for @list -> $v
is rw { $v = $v + 1 }` on its own — verified this simpler case DOES update
`@list` correctly) but is lost once the mutation happens inside a closure
whose call is deferred past the loop's own lifetime — likely the same class
of "captured container reference doesn't survive into a stored closure" gap
as other `ContainerRef` capture-vs-escape issues noted elsewhere in the
codebase (see `docs/adr/0013-container-interior-mutability-cellvalue.md` and
neighboring ADR-0018/0023/0025 lexical-capture work).

## Suggested next steps (not investigated further)

1. Minimize further: does the closure need to be pushed into an external
   `@callbacks` array (escaping the loop's own scope), or does it also fail
   if called from inside the loop body itself (before the loop moves to the
   next iteration)? This tells whether the bug is about escape/deferral
   specifically or about `is rw`-aliasing-through-closures in general.
2. Compare against the working direct-mutation case
   (`for @list -> $v is rw { $v = $v + 1 }`, confirmed correct) to find
   exactly which step differs once a closure creation is interposed.
3. Check whether this is the same underlying gap as
   `todo/tickets/native-pointy-param-is-rw-writeback-missing.md` (a sibling
   `given`/`with`-pointy-param `is rw` writeback gap for native types, filed
   the same session) — both are "an aliasing pointy/loop param's mutation
   doesn't propagate back to the source once a closure/deferred path is
   involved," though the `for`-loop case here is not native-type-specific.

## Reproduce

The repro above, no fixtures needed. Expected (raku): `11`, `21`,
`[11 21]`. Actual (mutsu): `11`, `21`, `[10 20]`.
