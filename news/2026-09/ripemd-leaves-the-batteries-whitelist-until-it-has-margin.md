# `Digest ripemd.t` leaves the batteries whitelist until it has margin

`main` failed the bundled-battery gate on three consecutive pushes — `b8300f9c`, `d3978843a`,
`9d810818` — and every pull request opened in that window inherited the failure:

```
REGRESSION: whitelisted 'Digest	ripemd.t' did not pass this run
GATE FAILED: a bundled library regressed below its recorded baseline.
```

Nothing was broken. `t/ripemd.t` passes, 9/9, with correct digests. It is only too slow for the
gate's hard `timeout 120` in `run_one`.

## The entry was added on evidence that did not travel

`Digest	ripemd.t` was whitelisted at 11:56 the same day, in #8459 — a genuine and substantial perf
fix, whose description states the rationale plainly:

> `Digest::RIPEMD`'s `t/ripemd.t` — the one bundled-battery test file too slow to whitelist
> (~113.7s against the batteries gate's hard 120s budget) — drops to **~73s local**, real margin
> under budget, and joins the whitelist.

That margin was a 12-core dev box number. Measured today on a 4-core remote container, with the
gate's own invocation against the bundled lib:

| measurement | machine | wall clock |
| --- | --- | ---: |
| before #8459 | 12-core dev box | ~113.7s |
| after #8459 | 12-core dev box | ~73s |
| today | 4-core remote container | **139.3s** (exit 0, 9 ok / 0 not ok) |

139.3 / 73 ≈ 1.9, which is about how much slower that container is than the dev box across the
board. So this is the same code on a slower machine, not a regression since #8459 — and the CI
runner is evidently in the same class.

## What changed here

One line out of `batteries-whitelist.txt`. The file still runs and is still reported; it just stops
gating. `batteries-exclude.txt` was deliberately **not** used: that list is for files whose verdict
is not a statement about mutsu at all (a third-party service, a self-racing harness), and its own
header says it is not a place to park a failing test. This file's verdict *is* about mutsu — it is
simply a verdict the gate cannot collect inside 120 seconds on the hardware it runs on.

## Why this is the narrow fix and not the real one

`t/ripemd.t`'s runtime is dominated by its last case, `rmd160('a' x 1_000_000)` — about 15,625
compression blocks with a `start` each, which is exactly the shape #7571 named. Making that fast is
the real fix; this change only stops one over-optimistic baseline entry from blocking every merge in
the meantime.

[#8471](https://github.com/tokuhirom/mutsu/issues/8471) tracks putting it back, and deliberately
raises the bar from "it passed once" to "it passes with headroom, on the slowest machine class that
runs the gate, and the number is recorded with the machine it came from".

## The part worth generalising

The whitelist is a **portable** claim — *this file passes* — but the evidence admitted for it was a
**local wall-clock number**, and nothing in the process required the two to agree. `ripemd.t` was
the only entry near the edge, so it is the only one that broke; a second one would get in exactly
the same way. #8471 carries that question (require a recorded margin, or stop making the budget a
bare wall-clock constant) rather than leaving it implicit in this unblock.
