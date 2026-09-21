# `ContainerizePair` gets a tag probe and one shared helper — and it is not a speed-up

`OpCode::ContainerizePair` rewrites exactly one shape: a named-flavour `Pair`
into the positional flavour (ADR-0021). ADR-0021 emits it for every
non-syntactically-named positional call argument, so it is among the most
frequently executed opcodes in any call-heavy program, and on the measured
`JSON::Fast` decode it has nothing to do in about 97.5% of its executions.

It existed in three copies — the interpreter dispatch arm, the Tier B shim
(`vm_jit_helpers.rs`), and `containerize_pair_item`, which the Slip path uses
because a Slip's elements bypass the compiler entirely. All three now go
through the one helper, and the helper asks the question with a pure tag probe
(`Value::is_string_pair_value`) before decoding anything. Tier B's generated
code already decided this with a single masked compare against `PAIR_PATTERN`,
a constant whose doc comment says it exists "for the `ContainerizePair` fast
skip"; the interpreter did a full `view()` — classify, `view_kind`, guard
construction, `ValueView` materialization — to reach the same answer.

`t/routines/call/containerize-pair-arg-probe.t` pins what the probe must and
must not match: a `Pair`-valued variable still binds positionally and keeps its
key and value, a syntactic bareword fat-arrow is still a named argument, and
`Int`/`Str`/`Array`/`Hash`/positional-`Pair`/`Match` arguments arrive
unchanged, including a `Match` slipped as an element. A probe answering for any
wider or narrower set than `ValueView::Pair` shows up there as an argument
bound to the wrong parameter. `raku` agrees with all fourteen assertions.

## The measurement, which did not go the way the ticket predicted

Two claims were made for this change when [#8993](https://github.com/tokuhirom/mutsu/issues/8993)
was filed. Neither survived.

**The lazy-`Match` hazard does not fire here.** The ticket argued that
`view()` forces a lazy `Match` (`nanbox/peek.rs`'s `Kind::Match` arm memoizes
the materialization), so passing `$/` as an argument paid for a materialization
the callee may never want. `MatchNode::force_attrs` increments
`vm_stats::record_regex_match_materialization`, so this is directly checkable,
and it reads 1000-of-1000 on every shape: with the call, without the call,
with the match merely assigned, and with `.match`'s result passed straight into
a sub and never stored. A `rust-gdb` breakpoint on `force_attrs` names the real
forcer in each case — the sink arm's own `view()` for the `~~` shapes, and
`dispatch_match_method` for `.match`, which forces where the Match is built.
Every Match-producing path in the tree forces before argument packaging ever
sees the value, so this opcode never meets a lazy one. The hazard those doc
comments warn about is real; this is not where it fires.

**The instruction count does not improve.** Paired callgrind on
`tmp/jf-decode.raku` at 100 records, `--profile profiling`, warm precompilation
cache (second run of each build quoted, per the warm/cold rule in
`.agents/skills/perf-tuning/SKILL.md` §0):

| build | Ir |
| --- | ---: |
| baseline | 3,602,428,531 |
| probe, helper not inlined | 3,607,832,328 / 3,607,400,502 / 3,606,173,546 |
| probe, helper `#[inline]` | 3,605,506,135 |

Turning an inlined match arm into a call cost more than the probe saved;
`#[inline]` recovered about a third of that and left the total still marginally
above baseline. The honest reading is that the difference is at the level of
whole-program run-to-run variance and that **nothing here is a win**: the
opcode runs ~15,000 times per decode against 3.6 billion instructions, so even
a generous per-execution saving is four decimal places down.

That is the same arithmetic as the [#8822](https://github.com/tokuhirom/mutsu/issues/8822)
evaluation this came out of, where argument packaging measured at 2.98% of a
whole run rather than the 36% that issue reported, and ADR-0110's Stage 2
profile puts the entire dispatch loop at 4.6% of a decode. An opcode's cost is
what its handler does, and this handler does almost nothing either way.

So the change is kept for what it demonstrably is — one implementation of one
conversion instead of three, the interpreter and the JIT provably agreeing on
how the question is decided, and a regression test that did not exist — and
carries no performance claim.
