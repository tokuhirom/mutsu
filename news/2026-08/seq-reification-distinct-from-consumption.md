# Seq reification and consumption are now distinct operations (ADR-0034)

A second method call on a lazy `Seq` used to throw where rakudo answers — even merely *asking
about* the value (`.defined`) was enough to destroy it:

```raku
my $b = "file.txt".IO.open(:r).lines;
say $b.defined;              # True (both)
say "Str: '" ~ $b.Str ~ "'"; # raku: 'A B C'   mutsu (before): X::Seq::Consumed
```

The root cause was that mutsu conflated rakudo's two `Seq` primitives — `.cache` (idempotent
reify, into the Seq, keeps it usable forever after) and `.iterator`/`.list` (steal the source,
once) — into one "materialize" operation that always did both at once. That forced a hand-maintained
patchwork of four disagreeing exemption lists and ten independent reify call sites, plus a
name-keyed env writeback band-aid that could not reach a value one call frame away or a second
alias of the same Seq.

[ADR-0034](../../docs/adr/0034-seq-reification-is-in-place-and-distinct-from-consumption.md) gives
`Seq` (and `HyperSeq`/`RaceSeq`) a real body — `Arc<SeqBody>`, reusing ADR-0030's `SyncUnsafeCell` +
generation-graveyard technique — with `Deref<Target = Vec<Value>>` so the ~330 existing
`ValueView::Seq` read sites needed no changes. `SeqBody::reify` (idempotent, in place) and
`SeqBody::take` (steal-unless-exempt) replace the old single "materialize", governed by one
`seq_method_consumes` table instead of four. `IO::Handle.lines`/`.words`'s separate
`ValueRepr::LazyIoLines` representation folded into a `SeqSource::IoLines` variant of the same
body, and the five global side tables that used to track cached/consumed/lazy state by `Arc`
address (a real leak, an untraced GC retention path, and an ABA hazard) are gone — that state now
lives on the body itself.

The headline repro above now matches raku exactly, and the ADR's own §1.3 alias-preservation
probes (`.cache` through a sub parameter, through a second alias, and a user `Iterator`'s
`pull-one` running exactly once across two non-consuming touches) are pinned by
`t/seq-reify-preserves-aliases.t`. The full raku-vs-mutsu consumption matrix — which methods
consume a Seq's single-use iterator and which merely reify it — is pinned by
`t/seq-consumption-matrix.t`, cross-checked against a real `raku` run of the same file.

One correction to the ADR's own design during implementation: measuring raku directly (rather than
just the `.iterator` case the ADR's oracle sampled) showed that a `Seq` built from a fully-known
literal list is *also* single-use by default (`my $s = (1,2,3).Seq; $s.List; $s.List` throws on
the second call in real raku) — not perpetually reusable as the ADR's §2.5 assumed. `SeqBody` now
tracks a `retained` flag, set only by a genuine non-consuming touch (or an explicit `.cache`), and
`take()` steals a body's first touch unless that flag (or `.cache`) already exempts it — matching
raku's stricter behavior, confirmed by `roast/S32-list/seq.t`.

One accepted, documented residual gap: mutsu's parser desugars the sigil array-context deref
`@$s` and an explicit `.list()` method call to the identical method-name string `"list"`, but raku
treats them differently (`@$s` never consumes; `.list()` does). Two pinned local tests exercise
opposite sides of this, so `reify_or_consume_seq_target`'s `"list"` handling carries a documented
compromise (steal a genuinely deferred source, never steal an already-reified body) until the
parser can tell the two call shapes apart — see `SeqBody::take`'s doc comment and
`t/seq-consumption-matrix.t`'s KNOWN GAP subtest.

Two unrelated bugs surfaced only by end-to-end testing along the way: the `...` sequence
operator's generator step could feed a just-produced `Seq` element back into itself to compute the
next element, silently consuming a value the caller's `$seq[N]` read also aliased (fixed by
reifying each generator step before storing it); and `Value::eqv`'s `Seq` comparison read a body's
elements directly via `Deref`, which saw an empty vec for a genuinely deferred, not-yet-pulled
body even after an explicit `.cache` (`.cache` itself is lazy) — fixed by reifying/consuming both
`eqv` operands in the VM's `exec_eqv_op` handler, which (unlike `Value::eqv` itself) has interpreter
access. The second was caught by `roast/S16-io/words.t`'s `is-eqv words(), <...>.Seq`.

The Miri probe module the ADR's migration plan called for (phase 5) was not implemented in this
PR — deferred to `todo/tickets/adr0034-phase5-seq-body-miri-probes.md`; it is soundness-probe
infrastructure, not a functional gap, and `t/`/roast already cover `SeqBody`'s behavior
extensively.
