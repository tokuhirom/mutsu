# The reflective latch was not "the worse of the two"

A measurement pass over what is left of [#7565](https://github.com/tokuhirom/mutsu/issues/7565)
after five slices (#7656, #7707, #7964, #8019, #8042, #8060). It ships no code:
the two findings are a retired blocker and a redesign that needs a decision,
recorded as [ADR-0092](../../docs/adr/0092-closure-capture-is-a-chained-tier-not-a-merged-copy.md).

## The method

Everything below is a warm-run callgrind instruction slope between 6 000 and
14 000 iterations of the ticket's loop, `MUTSU_JIT=off MUTSU_GC=off`, release,
taken from one build whose gates are all off by default so the variants share a
binary. "Tax" is the `use Test` line minus the same loop without it.

Baseline on `main` at `1a49a887`: floor 77 195, `+ use Test` 87 787, tax 10 592.
The gated build reads 77 395 / 88 183 / 10 788 — the gates themselves cost ~200,
which is why the table below compares within that binary rather than against the
clean baseline.

| gated out | floor | `+ use Test` | tax |
| --- | --- | --- | --- |
| nothing (gated baseline) | 77 395 | 88 183 | 10 788 |
| the reflective latch on `skip_env_write` | 77 386 | 88 126 | 10 740 |
| the captured-env merge | 73 518 | 83 352 | 9 834 |
| the whole capture machinery | 64 032 | 67 811 | 3 779 |

## Finding 1: the latch is dead, and it was named as a prerequisite

#7565's own body calls the process-global reflective latch "the worse of the
two" costs, on the grounds that one `EVAL` anywhere — including inside a module
the program never calls into — permanently makes every lexical store mirror
into the env. #8019's release note went further and named it as the thing
blocking the per-tier capture memo.

It is worth **57 instructions per iteration**: 0.5% of the tax, inside the drift
band #7964 documented for this area (four shapes of *identical* logic spanning
0.8%). And the memo it was said to block shipped in #8060 without it, because
the address the memo needed was never the right key — a key set is.

That matters beyond the number. A per-chunk `skip_env_write` is not a mechanical
change: mutsu's `EVAL` resolves against the live env chain rather than its own
lexical scope, so a callee's `EVAL` reads the caller's mirror and relies on it.
Someone was going to spend a large, correctness-critical change on this. Nobody
should, for this ticket's metric.

## Finding 2: the merge is the largest item, and it is a floor cost

Gating out the captured-env merge is 4 831 instructions per iteration with
`use Test` and 3 877 without — so only **954 of it scales with the import list**.
The rest is paid by any closure call in any program. The capture of a closure
with no free variables at all (`sub ($v) { $v + 1000 }`) is still 31 entries —
the built-in dynamics, `Any`, `?FILE`, the topic — and the merge probes every
one of them on every call, to insert none of them (#8019 measured `insert_sym`
reached zero times).

That reframes the item. It is not a `use Test` problem that shows up in a
benchmark; it is a per-closure-call cost of the interpreter that #7565 happens
to be where anyone measured. For scale, the whole capture machinery — build and
merge together — is 13 363 instructions per iteration on the floor, 17% of that
loop.

## Why no code, and what the ADR settles

Three mechanisms for removing the merge were worked through, and ADR-0092
records each in full. In short:

- **An identity token that lets the merge be skipped is unsound at any price
  worth paying.** The merge's precondition is a *superset* relation between two
  key sets ("every captured name is still visible"), and storing the closure
  into its own creating scope adds a key between capture and call — so exact
  equality never holds on this workload, and every cheap relaxation of it
  collides. Pinning `Arc`s certifies it soundly and costs an O(frame overlay)
  map clone per iteration, which is more than the merge.
- **Appending the capture at the chain's tail** needs no new field and no lookup
  change, but rebuilds every tier above it — reintroducing exactly the two
  `Arc::new`s per call that `Env::scoped_child` was tuned to remove, and adding
  a tier per call so `MAX_OVERLAY_DEPTH`'s flatten fires twice as often.
- **A `fallback` tier on the frame env** is the candidate, and it carries a real
  semantic change: capture entries stop being visible to *callees* of the
  closure, because they no longer sit in the frame overlay. That is arguably
  more correct — the current behaviour is dynamic scoping where Raku wants
  lexical — but it is a behaviour change with `EVAL` as its most likely
  dependant, and it is not a call to bake silently into a perf slice.

## What is left on #7565

1. **The no-op capture merge** — ADR-0092. 4 831 instructions per closure call,
   mostly floor. Largest item, and the only one whose fix is architectural.
2. **The remaining capture walk** — 43 candidates for 31 kept, at a hash probe
   per candidate; putting each candidate's resolved key and flags word in the
   memo beside it is worth roughly 650. Note that 650 is 0.7% of this loop,
   which is precisely the drift band #7964 measured for codegen shape in this
   filter, so it is only shippable with an ablation that separates the two.
3. **The ~20 built-in dynamics** — floor, not tax, and now measurably the same
   floor the merge pays for. A key-pure filter cannot tell an interpreter's own
   `init_io_environment` default from a user `my $*OUT = …` redirection, and a
   thread-bound `start` block needs the latter carried.

Item 2 of the previous list — the reflective latch — is struck, per finding 1.
