# The built-in dynamics leave the env

`Interpreter::capture_closure_env` keeps every visible env key that is not a
plain user lexical, because mutsu stores scalars sigil-less and so cannot tell a
user's `my $Foo` from a bare type name `Foo`. A closure created at the top of an
otherwise empty program therefore captured **23 entries, 20 of them the built-in
dynamic variables** the interpreter seeds once at startup — `$*OUT`, `$*ERR`,
`$*IN`, `$*ARGFILES`, `$*CWD`, `$*TMPDIR`, `$*HOME`, `%*ENV`, `@*ARGS`,
`$*PROGRAM`, `$*PROGRAM-NAME`, `$*REPO`, `$*SCHEDULER`, each under both its
`$*X` and its sigil-less `*X` spelling. They were re-filtered, re-inserted,
`Value`-cloned and later dropped on **every** closure creation: every
`.map({…})`, every `.grep({…})`, every callback literal in a loop.

And every one of them was then discarded unread. `call_compiled_closure_in_unit`
installs the closure's overlay over the *live caller env* and merges the
captured entries with `entry_or_insert_sym_with` — *don't overwrite what the
chain already has*. Every frame chain bottoms out at the interpreter's root env,
which holds those same dynamics for the whole program, so the captured copy of
`$*OUT` never won. They were not an over-broad approximation that happened to be
harmless; they were dead weight that was already never read.

[ADR-0086](../../docs/adr/0086-builtin-dynamics-are-not-closure-capture-material.md)
recorded that finding and the shape of the fix, and this change implements it:
the built-in dynamics move into a **per-interpreter, never-copied base tier**.

## What that is

`Env` grows a `dyn_base: Option<Arc<SymMap>>` read exactly where the process-wide
`GLOBAL_BASE` is read — at the chain's tail, after the overlay and every parent
tier. It is per *interpreter* rather than per process because these values are
not process constants: the IO handles and `$*PROGRAM`/`@*ARGS` belong to one
interpreter (Test::Util's `is_run` fast path runs a nested one in the same
process), and `$*CWD` is mutable. `GLOBAL_BASE` keeps the six that genuinely are
process constants (`$*PID`, `$*TZ`, `$*INIT-INSTANT`, `$*EXECUTABLE`,
`$*EXECUTABLE-NAME`, `$*SPEC`), and the lazily-materialized magic vars
(`$*VM`, `$*DISTRO`, …) still live in no env at all.

`Interpreter::hoist_builtin_dynamics` does the move, from `run()` — late enough
that `set_program_path`/`set_args` have already seeded `$*PROGRAM` and `@*ARGS`
— and again on every thread clone, so a spawned block's captures are no wider
than the parent's. The base map never changes after that call, which is what
lets every env share it by `Arc` with no copy-on-write; a *write* to a built-in
dynamic is promoted into the writer's own overlay by the existing
`Env::get_mut_sym` path, where it shadows the base exactly as an overlay entry
already shadows `GLOBAL_BASE`.

The soundness argument is structural rather than analytical, which is the point:
nothing decides that a closure "does not need" `$*OUT`. A closure that calls
`say` reaches `$*OUT` through a callee, not through a name the free-variable pass
could ever see — so the capture stops carrying the key because the key is not in
the env, and the read still resolves, through the tier, from wherever the closure
runs.

One refinement the design did not anticipate: the per-interpreter tier
**absorbs** `GLOBAL_BASE` rather than sitting beside it. Two base maps meant
every env *miss* — which is every metadata key the VM speculatively reads — paid
a second probe, and on `word-count` that cost more (~13M Ir in `Env::get_sym`
alone) than the capture saved. Merged, the tail of a lookup still costs exactly
one probe, as it did before the tier existed.

## What had to learn about the tier

Reads (`get_sym`, `contains_key_sym`) and the two promotion paths (`get_mut_sym`,
`remove_sym`) consult it at the tail; a flat env can now carry tombstones,
because with a tier below it a `remove` again has something to hide. Both chain
collapses (`flattened`, `filtered_flat`) carry the tier by reference and carry
forward any tombstone that still hides a base key. `flatten()` and
`visible_keys_where()` merge it as they already merged `GLOBAL_BASE`.

Two consumers iterate an env as *the visible environment* and had to be taught
explicitly, since `iter()` is map-only:

- `dynamic_pseudo_stash_entries`, which backs `DYNAMIC::` and `PROCESS::` —
  `PROCESS::<$OUT>` would otherwise have stopped finding the seeded handle.
- the thread-clone spawn walk, which collects the IO handle ids the child must
  keep working. Missing them made the child rebuild a handle it then could not
  find: a plain `await start { print "X" }` died with `Invalid IO::Handle`.

## Numbers

Callgrind instruction counts — deterministic and load-independent, so a local
A/B of two release binaries is meaningful where wall clock would not be. They
are not wall-clock figures and do not replace the bench CI history, which stays
the source of truth for tracked performance.

| | main | this | |
| --- | ---: | ---: | ---: |
| `my $c = * + 1;` × 200000 | 3,096,805,546 | 1,909,249,834 | **−38.4%** |
| — per creation, control loop subtracted | 13,503 | 7,564 | **−44.0%** |
| the same + 30 enclosing lexicals | 4,555,820,087 | 3,367,123,793 | −26.1% |
| — per creation | 20,798 | 14,853 | −28.6% |
| `sub make($n) { my $c = { $n + 1 } }` × 20000 | 1,048,977,489 | 917,072,113 | −12.6% |
| `bench-ctor` | 1,438,871,696 | 1,329,187,266 | −7.6% |
| `bench-class` | 1,185,541,115 | 1,146,133,720 | −3.3% |
| `bench-yaml-parse` | 597,848,254 | 584,912,644 | −2.2% |
| `bench-grammar-parse` | 47,889,416 | 47,396,313 | −1.0% |
| `bench-fib` | 1,230,729,592 | 1,230,738,762 | +0.0% |
| `bench-mandelbrot` | 792,256,437 | 793,310,309 | +0.1% |
| `bench-tak` | 1,521,783,637 | 1,525,319,286 | +0.2% |
| `bench-array` | 241,206,753 | 242,212,609 | +0.4% |
| `bench-string` | 504,742,311 | 506,998,734 | +0.5% |
| `bench-hash` | 264,406,272 | 267,143,291 | +1.0% |
| `word-count` | 1,058,126,579 | 1,076,057,078 | +1.7% |

The control loop with the closure creation removed is flat (+0.06%), so the
microbenchmark rows are the creation cost and nothing else. The sub-percent to
1.7% costs at the bottom are the price of the mechanism: an `Env` grown by one
`Option<Arc<_>>` and one more branch at the tail of a lookup, paid by programs
that create no closures. `word-count` is the worst case — a mainline-only loop
of hash-element increments, which is all env traffic and no capture.

## Acceptance

`t/vm/scope/dynamic-vars-base-tier.t` pins the behaviour the move must not change:
reads from a closure, from a callee and from a closure created inside a sub
frame; a `my $*CWD` shadow being what a closure actually captures; an assignment
inside a sub still visible after it returns; `%*ENV` still writable;
`PROCESS::`/`DYNAMIC::` still resolving; a lexically redirected `$*OUT` still
winning, including across a `start` block. All 18 assertions pass under rakudo
too. `t/vm/start-dynamic-var-indir.t`, `t/oo/class/start-inherits-dynamic-out.t`
and `roast/S32-io/indir.t` — ADR-0086's named acceptance tests — stay green.

#7557 stays open only for what ADR-0086 does not cover.
