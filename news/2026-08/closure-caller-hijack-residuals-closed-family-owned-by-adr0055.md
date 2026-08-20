# The closure-capture caller-hijack ticket is retired: every tracked residual is closed, and the family it named now belongs to ADR-0055

`todo/deep/closure-read-only-capture-loses-to-caller-env-same-name.md` was
opened on 2026-08-11 as the deep record of a single defect: *a closure's
read-only captured scalar is hijacked by a same-named lexical in the caller's
env chain, so lexical scoping degrades to dynamic scoping.* It was the origin
ticket for ADR-0025, and after that ADR's slice 1 landed it stayed open to
track four named residuals. On 2026-08-20 all four were re-verified against
`main` and the ticket was retired. This is that closeout.

The ticket is retired for two independent reasons, and it is worth keeping them
apart, because only one of them is "fixed":

1. **All four residuals it tracked are genuinely closed** — measured, not
   assumed (below).
2. **The headline defect is not fixed, but it is no longer a `todo/deep/`
   item.** It was re-diagnosed on the same day and adopted, with a complete
   five-slice design, by
   [ADR-0055](../../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md)
   ("A closure's free variable resolves to its own captured binding").
   `todo/deep/` holds findings that still need a design; this one has one.

## The four residuals, and how each closed

**Residual 1 — `http2-response-serializer.rakutest` "check 4", the cross-thread
race.** This was the hard one, and the ticket's own last word on it was a
warning: the failure is a genuine race (4/8 fresh runs failed with no code
change in between), instrumentation closes the window and produces false
"fixed" readings, and any claimed fix must be re-measured over 8-10 pristine
runs rather than one. Measured that way on `main` (debug build, un-instrumented),
the file is **10 pass / 0 fail out of 10** at 29/29 subtests. The race is gone.
It was not closed by anything aimed at it: the slice-2 close-out attributes it
to `2011b083b` (2026-08-19), which independently fixed the closure-dispatch
merge-site defect class the race belonged to.

**Residual 2 — `http2-request-parser.rakutest` test 49.** The ticket had already
established this was not a capture bug at all (a nested `whenever` made a
sibling event's `%streams{...}` write resolve to a stale forked container) and
had handed it to
`todo/deep/nested-whenever-registration-clobbers-sibling-event-aggregate-writes.md`.
That ticket was itself resolved by `c64222e9c` (a nested `whenever` keeps its
own supply's emitter) and `332487462` (a boxed hash/array element write no
longer takes the shared-store lane), and now lives in
`news/2026-08/nested-whenever-emitter-ownership.md` and
`news/2026-08/nested-whenever-shared-hash-cell-clobber-fixed.md`. The file
measures **4/4 runs at 61/61** on `main`.

**Residual 3 — ADR-0025 slice 2.** Picked up for implementation, re-verified
against `main` first, and closed with no code change: every premise had already
been fixed by intervening work (`cf9dc72be`'s unconditional
`method_escapes_closure_args`, the pre-existing `escaping_position` coverage,
and `2011b083b`). See
`news/2026-08/adr0025-slice2-closed-out-already-resolved.md`.

**Residual 4 — the session acceptance criterion.** Already marked resolved in
the ticket, with one loose end: `http-session-persistent.rakutest` "still fails
its own test 13 (`X::Cro::HTTP::Error::Client`) — a separate, undiagnosed
issue". That is closed too; the file measures **3/3 runs at 19/19**.

For completeness, the whole neighbourhood is green on `main`:
`http2-request-serializer` 32/32 (3/3 runs), `http2-response-parser` 9/9 (3/3),
`http-session-inmemory` 13/13 (3/3). All ten of these files are whitelisted in
`batteries-whitelist.txt`, so the release-time batteries gate now defends them.
The slice-1 pin `t/closure-capture-instance-cell.t` is green (7 tests). The
sibling ticket named at the bottom of the retired file,
`todo/tickets/closure-for-loop-param-hijacked-by-same-named-captured-outer.md`,
was also resolved (`news/2026-08/for-loop-param-getupvalue-hijack-fix.md`).

## What is *not* closed, and where it went

The defect in the ticket's title still reproduces on `main`. ADR-0055 §1.2(b)
states it in four dependency-free lines:

```raku
sub noop($v) { 1 }
my $b = "OUTER";
noop($b);                        # <-- the load-bearing line
my $f = { $b };
sub collide() { my $b = "CALLER"; my $g = { $b }; $g.(); $f.() }
say collide();                   # raku: OUTER    mutsu: CALLER
```

Verified 2026-08-20 on `main` @ `12b82920c`: `raku` says `OUTER`, mutsu says
`CALLER`. Without the `noop($b)` line both agree on `OUTER`.

This is the same family the retired ticket described, but the diagnosis has
moved on in a way that matters. The ticket blamed "a read-only capture gets no
cell". ADR-0055 identifies the load-bearing ingredient as a *vouch refusal that
cannot be made complete*: `compute_free_vars` refuses to vouch for any lexical
handed to a call, because an `is rw` parameter might write it back — so `$b` is
simultaneously not authoritative (handed to `noop`) and not boxed (never
mutated), leaving both defenses off. And it reframes the fix: caller-priority-
by-name is a *staleness* workaround expressed as a *scoping* rule, and that
category error is the actual defect. Its decision is that a closure's free
variable resolves to the binding the closure captured, on every call path, with
freshness delivered exclusively by the shared container cell — which retires
`merge_all`, demotes the three vouch sets from correctness gates to perf hints,
and collapses the two closure-state stores.

Anyone arriving here from the old ticket's title should read ADR-0055 and start
at its slice 1 (close the unboxed-mutated residue), which is the stated
prerequisite for everything downstream.

## Incidental: an ADR number collision, resolved

Two ADRs were authored concurrently on 2026-08-20 and both claimed 0054. The
argument-list-interpolation ADR held the `docs/adr/README.md` index row; the
closure one had merged without an index entry. It has been renumbered
0054 → 0055 and added to the index, and its three referrers
(`docs/adr/0025-captured-scalar-cells-value-kind-blind.md` and
`todo/deep/call-compiled-closure-lacks-merge-all-and-dual-persistence-store.md`)
were updated to match.
