# `GetLocal` stops paying a twelve-guard preamble on every local read

`OpCode::GetLocal` is the single most frequent opcode in any mutsu program —
several per statement — and until now every one of them ran the whole of
`exec_get_local_op_inner`'s guard chain before it could hand the slot to the
stack. [#8332](https://github.com/tokuhirom/mutsu/issues/8332) measured the bill
at **232 instructions per read** for a plain `my $x`, isolated as the difference
between two 40,000-iteration loops that differ by exactly one local read. That
is a flat tax on everything: `exec_get_local_op_inner`'s self cost was 6.78% of
`benchmarks/int-arith.raku`.

Twelve mechanisms live in that preamble — `$CALLER::x :=` aliases, CAS-modified
attribute locals, atomic variables, the `@`/`%` atomic-lane and thread-clone
probes, the env cell-adoption probe, the `self`-attribute cell read, the CStruct
`$!field` path, the type-object-invocant error, and the `HashEntryRef` /
`LazyThunk` / `ContainerRef` / `Nil` resolutions after the clone. Every one is
the right answer for some program. None of them is true for `my $x = 1; ... $x`.

## The fix was already written, for the other execution engine

The JIT's Tier B inline `GetLocal` (ADR-0004 J4d) had solved exactly this
problem in emitted code, and its analysis splits the eligibility in two:

- a **static half** decided by the slot's *name* — reject attribute slots,
  `!`/`.` twigils and `@`/`%` containers, and every name-shaped guard in the
  preamble is decided against;
- a **dynamic half**, the process-global monotonic `LOCAL_READ_SPOILERS` latch —
  zero proves that no `$CALLER::x :=` alias, atomic variable, shared cell or
  sigilless attribute alias has ever been created *anywhere in the process*, so
  the runtime-shaped guards are no-ops too.

The interpreter had no equivalent, so it re-derived all of it on every read. It
now shares both halves with the JIT rather than duplicating them:
`CompiledCode::local_read_plain` memoizes the static bit per slot (the way
`local_attr_key` already memoizes the attribute key), and
`vm_jit::local_read_unspoiled` reads the latch. A third condition,
`Value::is_plain_local_read`, is a pure tag probe excluding the kinds the arm's
*tail* still inspects (`ContainerRef`, `Proxy`, `HashEntryRef`, `LazyThunk`,
`Nil`) — a tag probe rather than a `view()` because a `view()` here would force
a lazy `Match` (ADR-0016 P5). When all three hold, the arm is
`stack.push(locals[idx].clone())` and nothing else.

The latch is monotonic and never cleared, so the mechanism can only ever turn
*pessimistic*: a program that creates a shared cell loses the fast path for the
rest of its run and gets the full preamble back. That is the property that makes
it sound by construction, and it is why the per-slot compile-time flag the issue
also floated was the wrong shape — the compiler cannot see a `:=` alias or an
atomic lane established at runtime from another frame, and a missed case there
would be a silently wrong read rather than a deterministic failure.

## One latent JIT divergence closed on the way

The spoiler set was documented as covering the env cell-adoption probe, but that
probe adopts a `Proxy` as readily as a `ContainerRef`, and only `ContainerRef`
was latched. A `Proxy` reachable from an env overlay under a name whose slot held
an ordinary value would therefore have been adopted by the interpreter and
skipped by the JIT's inline read. `Kind::Proxy` now bumps the latch at the same
NaN-box encode chokepoint `Kind::ContainerRef` uses, which closes that divergence
and is what lets the interpreter's fast path skip the probe as well.

## Result

Callgrind instruction counts, which are deterministic to ~0.01% across runs
(#8085) and are the metric the issue specifies. Per-read figures come from the
two 40,000-iteration loops it defines, differencing the
`exec_get_local_op_inner` row between the profiles; both loops independently
agree on the per-read self cost, so the two ways of reading the profile give the
same answer.

| `exec_get_local_op_inner`, per local read | before | after |
| --- | ---: | ---: |
| one extra scalar `GetLocal` (`$x`), `MUTSU_JIT=off` | 232 Ir | **57 Ir** |

What is left is the function's own call frame (~18 Ir), the latch and per-slot
loads, one tag probe, the `Value::clone` and the stack push. `@`-sigil and
attribute slots are statically excluded and are unchanged — they keep the full
preamble, which is what the sigil-keyed and cell-keyed probes are there for.

Whole benchmarks, both binaries built with the same profile so the counts are
directly comparable:

| benchmark | before | after | |
| --- | ---: | ---: | ---: |
| `int-arith`, `MUTSU_JIT=off` | 354,455,998 | 331,343,589 | **-6.5%** |
| `int-arith`, JIT on | 205,428,876 | 205,596,408 | +0.08% |
| `bench-ctor`, JIT on | 1,385,488,137 | 1,383,806,062 | -0.12% |
| `bench-index-read`, JIT on | 6,047,226,192 | 6,052,374,787 | +0.09% |

The JIT-on rows are flat by construction and are reported to show that: a hot
loop's local reads were already inlined by Tier B, so the only reads this path
sees there are the ones the JIT bailed on. The two small positives are the
latch load on reads that then fall through to the full chain anyway —
`bench-index-read` is `@`-sigil-heavy, so most of its reads are statically
ineligible. The guards are ordered cheapest-refusal-first to keep that residue
down; it is the price of the JIT-off column, and JIT-off is what every
non-JITted chunk in a JIT-on process runs.

Wall-clock for these lands in the bench CI history, not here.

`t/vm/binding/var-read-fast-path.t` pins the behaviour: the plain-read cases
first (the spoiler latch is process-global and monotonic, so the file's first
`:=` arms it for everything after), then one block per guard the path must
decline — `@`/`%` slots, public and private attributes, a deferred hash-entry
bind, a `:=` alias, a nested named sub's write-through cell, a `Proxy`'s `FETCH`
and an atomic variable. The whole file passes unchanged under Rakudo.
