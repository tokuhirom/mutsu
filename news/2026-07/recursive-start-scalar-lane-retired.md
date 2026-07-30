# Recursion through a `start` block no longer returns silently wrong answers

`sub f($n) { start { $n <= 0 ?? "b" !! await(f($n - 1)) ~ "|$n" } }` returned
`b|3|3|3` on its second call (raku: `b|1|2|3`), the `await start` variant was
wrong on the *first* call, and a two-branch recursive `fib` through `start` hung
deterministically. Root cause: `clone_for_thread` seeded every lexical into the
shared store under its **bare name**, which cannot represent two
concurrently-live bindings of one name — exactly what a recursive frame chain
is — and `await`'s frame-walking writeback then smeared the innermost value over
every ancestor frame.

The fix retires the name-keyed lane for a spawned block's **own captured plain
scalars** (`clone_for_thread_for_block`): `start` compiles its block as
escaping, so `box_captured_lexicals` already gives those scalars a correct
per-binding home (a shared `ContainerRef` cell when mutated, a frozen value when
read-only); the flat lane was a second, lossy mechanism overwriting the working
one's answer. Excluded names keep the `thread_redeclared_vars` mask on both
sides of the spawn — the old blanket mask-clear rested on the force-seed that no
longer happens, and dropping it let a stale pre-declaration Nil (written by the
blanket `sync_env_from_locals` mirror) be pulled back over a live binding at the
next `await`. That stale-Nil pull was exactly the coherence hole that
`roast/S17-promise/nonblocking-await.t` exposed in PR #4654's attempt (`$port`
went Nil → connect to port 0); with the mask retained the file passes.

Supporting fixes, each forced by a pin: type-constrained scalars are boxed when
the closure goes to a thread (`CompiledCode::thread_escaping`, transitive
through enclosing closures so `.map({ start { $c = $c + 1 } })` boxes the outer
`my Int $c`); `cas` swaps through a boxed scalar's cell (`scalar_cell_target`);
`.start` joins `then`/`tap`/`act` in `method_escapes_closure_args`; and lexicals
written by a registered class/role method keep the name lane
(`type_body_written_lexicals`) since the capture analysis cannot see those
writes.

This work was first attempted in PR #4654 (2026-07-17, pre-ADR-0010); that
branch fell 935 commits behind and targeted the retired process-global flat map,
so it was re-implemented from its investigation notes on top of the
lineage-scoped store. Its two regressions did not carry over: the day14 failure
was an artifact of the flat map, and the nonblocking-await hole is closed by the
mask retention above. Design record: `docs/recursive-start-shared-vars.md`.
New pins: `t/recursive-start-await.t`, `t/thread-shared-scalar-visibility.t`.
