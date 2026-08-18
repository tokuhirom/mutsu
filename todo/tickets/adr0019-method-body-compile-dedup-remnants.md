# `record_type_body_captures` runs a second, throwaway compile

Spun off from `todo/deep/adr0019-d3-8-method-body-main-pass-compilation.md` (ADR-0019 D3-8, now
closed). D3-8 landed the main-pass-compile-once mechanism for method bodies, but this call site
the original survey flagged still does a redundant compile; not a correctness bug, pure
architecture/perf cleanup (CLAUDE.md's "gain" sense — removing a duplicate mechanism), low
priority.

> **Update (2026-08-19): the ticket originally had two items; item 2 is resolved, this file is
> narrowed to the one still-open item.** Item 2 (`class_dispatch.rs`'s `compiled_holder`
> per-call recompile) turned out to be live and, via one caller, unboundedly hot — fixed the
> same day (`Registry::proto_compiled_cache`, PR #6655) and reverified afterward: the
> `compiled_holder` fallback itself is now confirmed to serve only its intended rare purpose.
> Full account in `news/2026-08/adr0019-compiled-holder-fallback-confirmed-rare.md`. That
> reverification also surfaced a related but distinct, more general finding (every hoisted
> class/role forward-reference shell pays a throwaway per-method compile that gets discarded
> unread) — spun off separately as
> `todo/deep/adr0019-hoisted-type-shell-throwaway-method-compile.md`, since it is
> higher-blast-radius than this ticket's low-priority scope and needs a correctness check before
> any fix. This file now tracks only the remaining item below.

`src/compiler/helpers_sub_body.rs` (`record_type_body_captures`, still present) runs a full
`compile_closure_body` per top-level method statement purely to harvest `free_var_writes` for
`type_body_written_lexicals`, then drops the compiled code — even though the main-pass compiler
(post D3-8) already compiles every method body once and keeps that bytecode in `CompiledFns`.
Merging this into the parity compile (single main-pass compile per method body, harvesting
captures as a byproduct) was explicitly deferred by D3-8's own design doc as future work, not
attempted there.

**2026-08-19 investigation (reconfirmed, unchanged):** the two compiles are not trivially
mergeable — they deliberately use different compiler contexts for different reasons.
`record_type_body_captures` compiles via `self.compile_closure_body` (the OUTER, main-pass
`Compiler`, with full access to the enclosing frame's lexical scope) specifically because its
job is to detect which OUTER lexicals the method body writes. `compile_method_body` (the D3-8
main-pass method compile it would merge with,
`src/compiler/helpers_method_body.rs`) compiles via a bare `Compiler::new()` that **deliberately
does not inherit** the outer compiler's scopes/fold_ctx/outer_code_var_names (see its own doc
comment, "design decision 2") — because it must byte-for-bit match the registration-time
throwaway compile, which also starts scope-blind. A merge would need `compile_method_body` to
somehow ALSO see outer-scope free-var info without breaking that parity guarantee — not
attempted this session (or the one before it); still open.

## Priority

Low — not a correctness bug. No safe, low-risk merge shape has been found across two
independent investigations; a real fix would need to either give `compile_method_body` outer-scope
visibility without breaking its registration-time-compile parity guarantee, or restructure
`type_body_written_lexicals` capture to not need a second compile at all. Left open rather than
forced.
