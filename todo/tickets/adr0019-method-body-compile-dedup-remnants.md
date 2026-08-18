# Two method-body compile paths still duplicate work D3-8's cutover was meant to make one-shot

> **Update (2026-08-19): item 2 was NOT a rare/dead corner — it was live, hot, and
> unbounded.** Investigated with the `method_body_runtime_compiles` `MUTSU_VM_STATS`
> counter (already wired for exactly this question — see
> `docs/adr/0019-compiled-declarations-and-unified-method-dispatch.md`'s D3-8 entry,
> which claims it "dropped to zero for ordinary class/role methods"). A sweep of every
> `t/*.t` file found **526 of them** with a nonzero count, several in the hundreds
> (`t/where-named-param-sibling-ref.t`: 265, `t/mustache-battery.t`: 191,
> `t/text-csv-battery.t`: 150). `rust-gdb -batch` breakpoints on
> `compile_method_def_in_place_with_dist` traced the dominant shape straight to
> `Interpreter::run_proto_method` (`src/runtime/dispatch_proto.rs`): every `proto
> method`/`proto submethod` call built a **brand-new synthetic `MethodDef` with
> `compiled_code: None` hardcoded**, so `run_resolved_method_celled`'s on-demand-compile
> path (the exact mechanism item 2 described) recompiled the SAME proto body from AST
> on **every single call** — not once per registration, not "before the owner's
> registration pass", but forever, unbounded by call count (265 recompiles for one
> 9-line recursive-`proto method` roast-shaped test).
>
> Fixed (this session): added `Registry::proto_compiled_cache` (keyed by
> `(owner, method_name)`, cleared whenever `set_proto_method` installs a new body for
> the same key so a class/EVAL redeclaration can never see a stale compile).
> `run_proto_method` now checks the cache before building the synthetic `MethodDef`,
> compiles once on a genuine miss, and caches the result for every later call.
> Verified: `where-named-param-sibling-ref.t` 265 → 1, `mustache-battery.t` 191 → 1,
> full `t/` suite (3242 files, 30036 tests) still green, `cargo clippy -- -D warnings`
> clean.
>
> **Not fixed, separate finding, needs its own investigation:** `t/text-csv-battery.t`
> (150), `t/role-pun-build-tweak.t` (21), `t/self-is-lexical-in-blocks.t` (26) stayed
> nonzero — a different call path, traced via the same gdb technique to
> `exec_register_role_op` → `Interpreter::compile_role_methods` →
> `compile_methods_for_map` (`src/runtime/accessors_resolve.rs`): every `RegisterDecl`
> execution of a role eagerly compiles all its method bodies via the same throwaway
> `Compiler::new()` machinery `compile_method_def_in_place_with_dist` uses, with no
> cache keyed on the role/method identity across separate registrations (e.g. repeated
> punning/composition of the same role). Unlike the proto case this did NOT show the
> same unbounded-by-call-count scaling in a quick check (a `RegisterRole` op fires once
> per registration, not once per method call), so it may just be "one throwaway compile
> per registration" — annoying but bounded — rather than a live per-call bug. Not
> root-caused to the same certainty as the proto fix above; a future session should gdb
> a hit-count sweep the way this one did before assuming it needs the same fix shape.

Spun off from `todo/deep/adr0019-d3-8-method-body-main-pass-compilation.md` (ADR-0019 D3-8, now
closed) before that design doc is retired. D3-8 landed the main-pass-compile-once mechanism for
method bodies, but two call sites the original survey flagged still do redundant compiles;
neither is a correctness bug, both are pure architecture/perf cleanup (CLAUDE.md's "gain" sense —
removing a duplicate mechanism), low priority.

## 1. `record_type_body_captures` runs a second, throwaway compile

`src/compiler/helpers_sub_body.rs` (`record_type_body_captures`, still present) runs a full
`compile_closure_body` per top-level method statement purely to harvest `free_var_writes` for
`type_body_written_lexicals`, then drops the compiled code — even though the main-pass compiler
(post D3-8) already compiles every method body once and keeps that bytecode in `CompiledFns`.
Merging this into the parity compile (single main-pass compile per method body, harvesting
captures as a byproduct) was explicitly deferred by D3-8's own design doc as future work, not
attempted there.

**2026-08-19 investigation:** the two compiles are not trivially mergeable — they deliberately use
different compiler contexts for different reasons. `record_type_body_captures` compiles via
`self.compile_closure_body` (the OUTER, main-pass `Compiler`, with full access to the enclosing
frame's lexical scope) specifically because its job is to detect which OUTER lexicals the method
body writes. `compile_method_body` (the D3-8 main-pass method compile it would merge with) compiles
via a bare `Compiler::new()` that **deliberately does not inherit** the outer compiler's
scopes/fold_ctx/outer_code_var_names (see its own doc comment, "design decision 2") — because it
must byte-for-bit match the registration-time throwaway compile, which also starts scope-blind. A
merge would need `compile_method_body` to somehow ALSO see outer-scope free-var info without
breaking that parity guarantee — not attempted this session; still open.

## 2. `class_dispatch.rs`'s `compiled_holder` still recompiles into a local clone per call

`src/runtime/class_dispatch.rs` (~line 562, `compiled_holder: Option<MethodDef>`) builds a local
owned clone and compiles into it, discarding the result after the call, on one dispatch path.
D3-8's design doc explicitly named persisting this "residual fallback" as an accepted risk, not a
fixed one — confirm at investigation time whether this path is actually still reachable post the
Phase E dispatch-resolver unification (E1-E11) before assuming it's live traffic; it may now be a
rare/dead corner the resolver bypasses.

## Priority

Low — neither is a correctness bug (unlike the D3-8 lexical-capture bug this same survey also
flagged, which turned out to already be fixed by the time this ticket was filed — reverify before
assuming either of the two items above is still live, the same way that one was reverified stale).
