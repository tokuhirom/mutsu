# `start` blocks no longer force a routine onto the interpreter fallback

`Interpreter::function_body_needs_interpreter` treated *any* `start { ... }` call appearing in a
routine body as proof that the routine had to tree-walk rather than be OTF-compiled to bytecode:

```rust
Expr::Call { name, args } => {
    // start blocks need the interpreter for proper thread spawning
    name.resolve() == "start" || args.iter().any(Self::expr_needs_interpreter)
}
```

That blanket exclusion is gone. Investigating `todo/deep/start-block-otf-compilation-gate.md` turned
up three facts that together make it indefensible, and none of them required new capture machinery.

## The premise the ticket inherited was stale

The ticket recorded the gate as the regression pin for `t/start-block-return-value.t` test 3 — a
recursive sub whose `start` closure captures a parameter that the recursive call re-binds — and
proposed "per-call capture cells" as the prerequisite for narrowing it. Measurement says otherwise.
`MUTSU_VM_STATS=1` on that file reports fallbacks only for the `start` and `await` builtins
themselves; `conc-fib` and `foo` never fall back. They are ordinary single subs, so they are gated by
`def_is_otf_compilable_module_single`, which has admitted `start`-containing bodies since ADR-0019
C6e-2c: the compiled caller-env merge already excludes the callee's own parameters
(`routine_writeback_excluded_names`), so an invocation's binding stays isolated from the thread env
its spawned closure reads. The pin has been running on the compiled path for over a month, and its
protection comes from the C6e-2c writeback fix, not from this gate.

## What the gate actually still covered

Only the arms that consult `RoutineBodyFacts::needs_interpreter`: multi candidates
(`def_is_otf_compilable_multi_candidate`), protos, and genuine builtin shadows
(`def_is_otf_compilable`). So byte-identical bodies compiled or tree-walked purely according to
whether they were declared `sub` or `multi sub` — an arbitrary split, not a safety property.

It was also leaky in the other direction. The walk only descends `Stmt::Expr`, so it saw `start` in
expression-statement position (or inside a call/method-call argument) and nothing else. The most
common way to write one, `my $p = start { ... };`, is a `Stmt::VarDecl` — invisible to the gate, and
therefore already compiling.

## Evidence for the removal

A probe with recursive `multi`/`proto` routines whose `start` closures capture a parameter (a fib
fan-out, a `Str` label read *after* the recursive `await`, a `[+]`-over-siblings fan-out, and `start`
as a call argument) produces raku-identical output. `MUTSU_VM_STATS=1` A/B on that probe shows the
change is real rather than a no-op: with the gate on, `mfib=176 fanoutm=30 pf=5 tagm=3` fall back to
the tree-walk path; with it removed, none of them do, and the results are unchanged.

Verification: the full `t/` suite (3336 files, 31003 tests) and `make roast` (1436 files, 218836
tests) both pass, plus three `-j4` release sweeps of all 99 whitelisted `roast/S17-*` concurrency
files and five release repeats of every `start`-related `t/` file.

`t/start-multi-candidate-compiled.t` pins the newly-compiled shapes.

## What remains

`function_body_needs_interpreter` now reports true only for a top-level `class`/`role` declaration,
with `expr_needs_interpreter` reduced to recursion into statement-hosting expression positions (`do`,
a bare block, and those as call arguments) so a nested declaration is still seen. Whether that last
exclusion is also stale — `def_is_otf_compilable_module_single` has admitted nested class/role decls
since the §3 fallback removal — is a separate question with separate evidence to gather, and was left
alone here.
