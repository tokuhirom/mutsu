# A container mutated from a nested frame reaches its owner, not a same-named shadow

`todo/deep/module-file-scope-array-and-hash-still-share-the-caller.md` had been
narrowed, over several passes, to exactly one remaining requirement — ADR-0039
§6's acceptance row (a):

```raku
sub f {
    my @a = 1, 2;
    my $push = sub { @a.push(9) };
    { my @a = 3; $push(); say "inner=", @a }   # want inner=[3]
    @a
}
say f();                                       # want [1 2 9]
```

That row now answers byte-identically to `raku`, and so does every shape around
it. The file is closed; `t/nested-frame-container-mutation-reaches-owner.t`
pins seven of them so it cannot rot back into a to-do item.

## What was wrong, and what fixed it

A container mutated from a nested frame propagated to its owner **by name**. The
mutating frame has no slot for `@a`, so the write landed in `env` — where the
inner block's own `my @a` was sitting. The closure wrote the shadow and the
owner's binding was left untouched. Worse, when the write *replaced* the
container instead of mutating it in place (`@a = 7,8`, `@a .= sort`, a shrinking
`.shift`), the owner's slot went stale even with no shadow anywhere in sight.

Nothing in this campaign fixed it. It was closed by
[`da8e94252`](https://github.com/tokuhirom/mutsu/commit/da8e94252) — ADR-0055
slice 1b, "an escaping container capture the frame cannot vouch for gets a cell"
— which put the container in a shared `ContainerRef` cell at its declaration, so
the closure holds the *binding* rather than the *name*. Verified by building at
`da8e94252^` and running the probes there: all six shapes diverge before it and
match `raku` after.

| shape | at `da8e94252^` | today | raku |
|---|---|---|---|
| `.push` from a nested sub, shadow live | `inner=[3 9]`, owner `[1 2]` | `inner=[3]`, owner `[1 2 9]` | same as today |
| whole-container replace (`@a = 7,8`) | owner `[1 2]` | owner `[7 8]` | `[7 8]` |
| shrinking `.shift` | owner `[1 2 3]` | owner `[2 3]` | `[2 3]` |
| hash key add | owner `(a)` | owner `(a b)` | `(a b)` |
| hash replace | owner `(a)` | owner `(z)` | `(z)` |
| `.=` rebuild | owner `[3 1 2]` | owner `[1 2 3]` | `[1 2 3]` |

## What this unblocks

The closed file was not merely a bug report — it was the recorded **blocker for
ADR-0039 slice 2**, the flip that makes an `@`/`%` read compile to
`GetLocal(slot)` instead of a by-name env lookup. Slice 2's read side had been
implemented and measured twice; `prove t/` went fully green under it once four
store-side defects were fixed (ADR-0039 §10.2), but `make roast` then failed two
whitelisted files — `roast/S15-nfg/concat-stable.t` and
`roast/integration/advent2014-day05.t` — and both were this same root cause. The
by-name read was hiding it, which is why the flip could not land ahead of it.

Both files pass today, and the root cause is measured closed, so **slice 2 is
re-attemptable** and is now tracked on its own as
`todo/deep/adr0039-slice2-container-reads-compile-to-a-slot.md` rather than
buried under a symptom that no longer reproduces. Whoever picks it up should
still expect ADR-0039 §10.2's four store-side defects, and should re-measure them
first — they were enumerated against a build from before `da8e94252`, so some may
have gone the same way this one did.

## Also still open

One row of the old exclusion-list matrix diverges and is a **scalar** lane bug,
not a container one: a module's `my $anon = [...]` colliding with a consumer's
`my $anon`. It stays at
`todo/tickets/module-scalar-held-array-collides-with-caller-my.md`.
