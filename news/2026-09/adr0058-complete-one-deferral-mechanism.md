# ADR-0058 is complete: one deferral mechanism, not two

Step 4 retires `create_lazy_map_list` and the `body_contains_return` predicate —
102 lines deleted — and with them the second of the two mechanisms mutsu used to
defer a `.map`/`.grep` callback. `SeqSource::MapGrep` is now the only one.

## The carve-out was already obsolete

Both `dispatch_map_method` and `builtin_map` kept a callback whose body contains
`return` on the older `LazyList` route, with this reason on the record:

> that `return` targets the lexically enclosing routine, and if the Seq is
> forced after that routine has exited it must surface as
> `X::ControlFlow::Return` with out-of-dynamic-scope set, which the `LazyList`
> path already gets right.

Measured against rakudo before and after removing it, the three shapes that
exercise the claim are unchanged:

| probe | rakudo | before | after |
|---|---|---|---|
| `sub f() { my $s = (1,2,3).map({ return 9 }); "made" }` | `made` | `made` | `made` |
| forcing the Seq after the declaring routine returned | throws `X::ControlFlow::Return` | same | same |
| `sub h() { my $r = (1,2,3).map({ return 9 }).List; "unreached" }` | `unreached` | `9` | `9` |

So `SeqSource::MapGrep` surfaces the out-of-dynamic-scope return identically,
and the reason to keep a whole second mechanism had lapsed at some point without
anyone re-measuring it. That is the same shape as two other findings from this
ADR's own steps — `Value::truthy`'s comment promising a boolean-chokepoint force
that did not exist (§9.5), and `env_root_descended_mut_tracked`'s doc comment
telling callers about to mutate to use it while the chained-subscript store
called the untracked sibling. **A comment stating an invariant is not evidence
the invariant holds.**

## One divergence measured, and deliberately left open

Row 3 above is a real difference and it is *not* caused by step 4 — it answers
`9` with the carve-out present and absent alike. A `return` inside a `.map`
callback forced **within** the declaring routine returns from that routine in
mutsu; rakudo runs on. Recorded in ADR-0058 §1b rather than fixed here, because
it is a `return`-targeting question, not a deferral one.

## Gates

`make test` PASS (3799 files / 40048 tests) · full local `make roast` PASS
(1436 files / 218962 tests) · `scripts/battery-testsuite.sh` **GATE PASSED**
(289/312) · `cargo fmt` / `clippy --all-targets -D warnings` clean.

## ADR-0058, start to finish

Every step shipped on 2026-09-07:

| step | what |
|---|---|
| 2 | `SeqSource::MapGrep` + the `pull_seq_source` arm, `dispatch_map_method` only |
| 3a | the listop `map &f, @xs` form |
| 3c | `@a.map` on a real array — three eager implementations, all keyed on the `@` receiver |
| 3b | every `grep` entry point |
| 4 | retire `create_lazy_map_list` |

The process lesson is §9.5's, and it is the part worth carrying forward: a green
gate on the step that deferred is not evidence its consumers are covered,
because a still-eager sibling keeps handing out reified Seqs that hide the same
read-path holes. Deferring `grep` is what surfaced five of them in `.map`.
