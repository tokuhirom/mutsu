# `.map` produces a deferred `Seq`: the callback runs at first consumption

[ADR-0058](../../docs/adr/0058-map-grep-produce-a-deferred-seq.md) step 2 shipped.
`.map` (the method form, `dispatch_map_method`) no longer runs its callback at
the call: it hands back a `Seq` whose `SeqSource::MapGrep { items, func, fatal }`
body runs the callback the first time something consumes the sequence, through
ADR-0034's existing `reify`/`take`/`sink` split. That is rakudo's timing, and it
closes the whole "residual try-cell" divergence family in one move:

```raku
try { (1..3).map({die "boom"}) }; say "alive ", $!.defined
# before: alive True     after (and raku): dies, uncaught
my $s = (1..3).map({ say "side $_"; $_ }); say "before"; say $s.List;
# before: side 1/2/3 then "before"     after (and raku): "before" first
```

Nine of the eleven `todo` rows in `t/map-callback-runs-at-consumption.t` — the
ADR's completion oracle, raku-verified 23/23 — are now un-`todo`d. The two that
remain belong to a separate, narrower bug (a force-time `fail` under an
enclosing `try` returning a `Failure` instead of throwing), still tracked in
`todo/deep/residual-try-cell-eager-seq-reification-divergences.md`.

The interesting half of the work was the read path. ADR-0034 §2.1 deliberately
made a `SeqBody` read through `Deref` return the *empty seed* for a body nobody
has pulled, so a read can never re-enter the VM — safe while deferred bodies
were a rare `IO::Handle.lines` corner, and load-bearing the moment every `.map`
became one. `Interpreter::reify_map_grep_seq`/`_args`/`sink_map_grep_seq` are
the new guard (tag-probed before `view()`, and a no-op for every other
`SeqSource`, whose streaming semantics must not be forced at an argument
boundary); ADR-0058 §8.2 lists every call site by funnel — argument boundaries,
rendering and coercion, operators, `@`/`%` assignment, composition and hyper,
the `Test` handlers that discard a block's value, and the program's own tail
statement.

Three of the ADR's own premises did not survive contact with the code, and each
turned out to be a real bug rather than a detail (ADR-0058 §8.3): `SeqBody::take`
never stored what it pulled, so `.iterator` and the hyper dispatch read the same
body back through `Deref` and got nothing — a latent ADR-0034 bug that made
`method iterator() { self.pairs.iterator }` in a `does Iterable` role iterate
zero elements; `use fatal` is lexical, so deferring the loop moved that read
from the `.map` call site to the consumer's dynamic context and both created and
suppressed exceptions (the source now captures `fatal` at the call); and the
pull is the effective call site for the callbacks it runs, so it has to drain
`reconcile_caller_after_lazy_force` exactly as `force_lazy_list_vm` does, or a
`LAST $ran = True` in the callback never reaches the consuming frame's slot.

Step 0's own recipe is worth recording as a lesson: the ADR prescribed measuring
the read-path exposure with a *proxy* (routing every map through
`create_lazy_map_list` behind a throwaway env gate). It over-reported — 59 `t/`
files and 55 roast files versus 44 under the real mechanism — and its excess was
systematic, not random: half the extra hits were the concurrency family failing
on option 1's per-`map` `Env` clone, a defect `SeqSource::MapGrep` does not have.
Run an exposure probe with the mechanism you intend to ship, behind a gate; it
costs nothing extra, because the mechanism has to be written anyway.

`grep` and the `map &f, @xs` function form stay eager for now (ADR-0058 step 3),
and the `body_contains_return`/`is_stub_routine_body` deferral predicate stays
until step 4.
