# The `S17-procasync/stress.t` SIGSEGV is not the cross-thread container-write class

ADR-0068 §3.1 carried the 2026-07-30 CI SIGSEGV (run 30590633128, the
rakudo#3299 block of `roast/S17-procasync/stress.t`) as an unexplained loose end
of the cross-thread container-write campaign, and named the cheap next step:
run the file under §1.1's harness at 24-way, and run §1.2's oracle over it to
see whether its containers are on the lane at all. Both are now done, and the
answer is **no** — the file has essentially no aliased container mutation, from
any thread, so this campaign's class cannot be its cause.

## The oracle says there is nothing there

There is one instrumentation wrinkle that is very likely why earlier hunts
learned nothing from this file: most of its blocks run their program in a
**child** process, through `is_run` / `doesn't-hang`. A breakpoint on the parent
`prove -e mutsu` sees none of them. So each block was extracted and run
directly.

| probe | `ContainerStructGuard` | `shared_array_elem_set` | unsynchronized aliased store | `gc_contents_mut<ArrayData>` |
|---|---|---|---|---|
| rakudo#3299 block (1200 → 100 `Proc::Async` in a `react`) | 0 | 0 | 0 | **1** |
| block 1 (400 `Proc::Async`, `.tap` writing a captured `$output`, `@got.push`) | 0 | 0 | 0 | **1** |

The single `gc_contents_mut` call in each is not a thread doing anything. Its
backtrace is `exec_set_local_op` → `stamp_descriptor_name` for the `my @target`
/ `my @got` declaration, on the main thread, before any `Proc::Async` exists.

Block 1 is the instructive one. It *looks* exactly like ADR-0068 route 1 — a
`.tap` callback closing over `$output`, with `@got` pushed from the main loop —
and it is not on the lane at all: `$output` is a `Str`, not a container, and the
`@got` push is unaliased, so it never reaches the primitive. A route's shape is
not evidence that it is on the racing path; the oracle is.

## And it does not reproduce at oversubscription

Debug build, `MUTSU_GC=on MUTSU_GC_EVERY_CANDIDATE=1024 MUTSU_GC_VERIFY=1`,
24-way on 12 cores — the threshold ADR-0068 §1.1 identifies as the necessary
ingredient, and the one every earlier hunt ran below:

| workload | result |
|---|---|
| the whole `roast/S17-procasync/stress.t` through `prove` | **0 / 48** |
| the rakudo#3299 block as a standalone program | **0 / 48** |

## Consequence

ADR-0068 gains §12 and §3.1 is answered. The crash is still real and still
unexplained — a SIGSEGV is memory unsafety somewhere, and it should not be
written off as a flake — but it is tracked on its own now, as
[#7609](https://github.com/tokuhirom/mutsu/issues/7609), which records the
candidates this measurement does not exclude: the `react`/`whenever`
tap/serialize-group bookkeeping under 1200 rapid setup/teardown cycles,
`signal(SIGTERM)` handler registration churn, and `Proc::Async` reaping state,
the one thing genuinely shared across all 1200 iterations.

With §11's route-5 classification and this section, nothing in ADR-0068 §3 is
Exposed-and-unmeasured any more.
[#7543](https://github.com/tokuhirom/mutsu/issues/7543) is left with one item:
the §2 lane-decline reasons no route has yet exercised — a twigil'd name, a name
masked as re-declared, and a container that was never in a spawning frame's env.
