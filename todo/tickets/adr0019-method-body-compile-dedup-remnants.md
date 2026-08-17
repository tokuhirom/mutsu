# Two method-body compile paths still duplicate work D3-8's cutover was meant to make one-shot

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
