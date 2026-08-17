# callsame from an override of gist/Str/raku returns Nil instead of reaching the native implementation

Found by the ADR-0019 E9-pre raku verification campaign (2026-08-12, Rakudo v2026.06).
Narrowed 2026-08-17: the `new` half of this ticket is FIXED (see
`news/2026-08/callsame-native-mu-new-fallback.md` — the "Fallback: if we are inside a
`new` method" block in `dispatch_next_candidate`, `src/runtime/builtins_dispatch_next.rs`,
used to trigger only for `nextwith`/`callwith`, never the bare `nextsame`/`callsame` forms).
This ticket now covers only `gist`/`Str`/`raku`.

## Divergence

```raku
class C { method gist() { "custom+" ~ callsame } }
say C.new;
# raku:  custom+C.new          (callsame reaches Mu.gist)
# mutsu: "Use of Nil in string context" warning, then "custom+"
```

Same shape for `method raku()` and `method Str()` overrides, for a role-composed
override (`role R { method gist() {...} }; class C does R {}`), and for a `multi method
gist()` override — all four shapes reproduce identically (verified 2026-08-17).

## Root cause (found 2026-08-17, root-caused precisely; NOT fixed — see below)

`dispatch_next_candidate`'s exhausted-MRO fallbacks (`native_mu_base_next_candidate` for
BUILDALL/POPULATE/clone, the `new`-bless fallback) all key off
`self.samewith_context_stack.last()` to learn "what method is currently executing" —
but **a single (non-multi, non-wrapped) compiled method call never pushes a
`SamewithContext` at all**. `push_method_samewith_context` is called from exactly three
sites: `vm_call_method_compiled.rs`'s wrap-chain branch (gated on
`self.has_any_wrap_chains()`), and `class_dispatch.rs`'s slow/interpreter-path dispatch
(reached for constructor calls like `new`, and other paths that route through the
tree-walk method dispatcher). The COMPILED fast path
(`call_compiled_method`/`call_compiled_method_fast` in `vm_method_dispatch.rs`) — the one
an ordinary `method gist() {...}` override with no wrap/multi/role complications takes —
pushes `method_class_stack` (just the class name) but never a `SamewithContext`, so
`samewith_context_stack.last()` is empty and any exhausted-MRO fallback keyed off it
silently returns `None` before ever checking `method_name`.

Confirmed via `rust-gdb`: breaking at the top of a would-be `gist`-fallback function, the
breakpoint fired but `self.samewith_context_stack.last()` was `None`, so the function
returned via its `?` before reaching the method-name match. The identical construction
for `new` DOES work because constructor dispatch goes through the slow
`class_dispatch.rs` path (which pushes a `SamewithContext` unconditionally), not the
compiled fast path.

## Why this needs a bigger fix than the `new` one

`method_name` IS available as a plain parameter at the top of both
`call_compiled_method`/`call_compiled_method_fast`, so in principle a `SamewithContext`
could be pushed there too. But:

- Both functions are **hot paths** (every method call in the interpreter goes through
  one of them) with **6 separate `pop_method_class()` exit points** across
  `vm_method_dispatch.rs` (lines ~435/579/791/910/1358/1754 as of this writing). A
  new paired push/pop would need to mirror every one of those exits correctly, or risk
  leaking/misaligning `samewith_context_stack` entries for calls that DON'T touch it
  today.
- An **unconditional** push (cheapest to get right) costs a String clone + Vec clone on
  every compiled method call, including the overwhelming majority that never call
  `callsame`/`nextsame` — a real regression for a path this hot (see the file's own
  comments about avoiding "a redundant... pull" on the common case).
- A **conditional** push (only for methods whose body might call
  `callsame`/`nextsame`/`nextwith`/`callwith`) needs a compile-time flag on
  `CompiledCode`, mirroring existing flags like `has_once`/`uses_callframe` — no such
  flag (`uses_callsame`/similar) exists yet, and computing it requires a body scan at
  compile time, not just a name-based check like the `new` fix's `in_new` test.

## Suggested fix shape

1. Add a `CompiledCode` flag (compile-time, body-scan-detected) for "this method body
   references callsame/nextsame/nextwith/callwith" — call it e.g. `uses_dispatcher`.
2. Gate a `push_method_samewith_context`/`pop_method_samewith_context` pair on that flag
   at the SAME entry/exit points `push_method_class`/`pop_method_class` already bracket
   in both `call_compiled_method` and `call_compiled_method_fast` (reuse those exact
   6+2 sites so no new exit-path bookkeeping is needed).
3. Then a `native_any_base_next_candidate`-shaped fallback (mirrors
   `native_mu_base_next_candidate`) can read `samewith_context_stack.last()` for
   `gist`/`Str`/`raku` and dispatch to `self.try_native_method(&invocant, method_sym,
   &args)` — this part was prototyped and confirmed correct in isolation (verified it
   fires and returns the right value once a `SamewithContext` is present), just never
   reachable without step 1-2.

Alternatively: re-verify at the ADR-0019 E9 cursor-design boundary
(`todo/deep/adr0019-e8-e11-candidate-sequence-semantics.md`) if that lands first — its
stated goal is "all four fallbacks [become] ordinary sequence tail entries", which would
plausibly subsume this gap by construction.
