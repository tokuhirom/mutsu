# `cas` counts as a write, so a captured lexical gets its shared cell

`todo/tickets/cas-on-instance-valued-scalar-forks-from-a-closure-capture.md`
reported that a closure created before a `cas` never observes the swap:

```raku
class Node { has $.k }
my $head = Node.new(k => 1);
my $obs  = { $head.k };
cas $head, $head, Node.new(k => 2);
say $obs();     # raku: 2    mutsu: 1
say $head.k;    # raku: 2    mutsu: 2   (the direct read was always fine)
```

## The ticket's root cause was wrong

The ticket blamed `Interpreter::atomic_scalar_cell`'s value-kind skip list: it
still carries `ValueView::Instance`, so a `cas` on an Instance-valued scalar
falls through to the name-keyed legacy lane while the capture side holds a
shared cell — two lanes that never see each other's writes.

Re-measuring first showed that reading was incomplete. The same divergence
reproduces for a plain **Int**, which that skip list does not refuse:

```raku
my $x = 1; my $o = { $x }; cas $x, $x, 5; say $o();   # raku: 5   mutsu: 1
```

A `gdb` breakpoint on `atomic_scalar_cell` confirmed it: for the Int repro it
*did* hand back a cell. The `cas` wrote through a cell; the closure simply was
not sharing it, because it had never been given one.

## The actual cause

The compiler decides which locals need a shared cell from `self_mutated` — the
set of names this frame writes. An atomic op is a `__mutsu_*_var("name", …)`
call, which the opcode scan cannot recognise as a write, so
`note_atomic_env_sync_target` folds those names in explicitly via
`atomic_target_syms`. Every atomic op passed `counts_as_write = true` there —
**except `cas`**, deliberately, on the grounds that its cross-thread behaviour
rides on the name-keyed lane that cell promotion would take away.

That exclusion is what starved the capture: `$x` never entered `self_mutated`,
so `box_captured_lexicals` saw no captured-and-mutated local, and the closure
kept a by-value capture. The hazard the exclusion guarded against is now
covered by `legacy_atomic_lane_owns` (2026-08-28,
`news/2026-08/atomic-cell-shape-refusal-asymmetry-resolved.md`), which makes
the *capture* side decline while a legacy lane is live.

## The fix

All three `cas` compile sites pass `counts_as_write = true`, matching every
other atomic op. Both lanes then resolve to the same cell before
`atomic_scalar_cell` is ever consulted — which is why the Instance case is
fixed too, with the skip list untouched.

The ticket's second question — the two skip lists' comment still claims they
mirror each other, though they have diverged on `Instance`, `Package`, `Array`
and `Hash` — is answered by correcting the comment rather than unifying them.
`atomic_scalar_cell` stays deliberately wider: a refusal there is cheap (the op
falls back to the legacy lane), while a refusal in `box_captured_lexicals`
costs the closure its cell. The two only have to agree on
Seq/HyperSeq/RaceSeq/Slip.

## Coverage

`t/cas-captured-lexical-coherence.t` (12 assertions): the ticket's Instance
repro, the typed variant, the Int variant, a *failed* `cas` publishing nothing,
repeated swaps followed by a plain assignment, and the `⚛++` / `⚛=` ops that
were already coherent. The whole file passes under `raku` as well as mutsu.
`roast/S17-lowlevel/cas.t`, `t/atomic-cell-shape-refusal-symmetry.t` and
`t/cross-thread-shared-var-writeback-coherence.t` — the acceptance surface the
ticket named — stay green.
