# A `sub` declared inside a block no longer falls through the slow call ladder on every `.()` invocation

A `sub` declared directly inside a bare block/closure body — `my $blk = {
sub foo () { 42 }; ...; foo }` — used to skip the compiled-function fast
path on *every single call* to `foo` whenever the block itself ran through
`call_compiled_closure` (the path `.()` and `call_sub_value`'s compiled-code
machinery reach). Each call instead fell through the full
`user_function_matches_call` resolution ladder in `dispatch_func_call_inner`
(`src/vm/vm_call_func_ops.rs`): a fresh `resolve_function_with_types`, an
OTF-compilable gate check, `push_samewith_context`/`push_multi_dispatch_frame`
setup, and (for a `state`-declaring nested sub) outright rejection by the
OTF gate's `declares_state` exclusion, degrading to full tree-walk
`interpreter_fallbacks` dispatch on every call.

## Root cause

The block's nested `sub` genuinely was compiled once and attached to the
closure's own `compiled_fns` table (`SubData::compiled_fns` /
`CompiledCode::compiled_fns`, ADR-0019 C6e-3c) — that plumbing was already
correct. The bug was a compile-time/runtime **package name mismatch** in how
that table's key was built:

- At compile time, a `sub` declared directly inside a closure/block body is
  compiled while `Compiler::current_package` holds the closure's own
  *synthetic state-scope pseudo-package* (`Pkg::&<closure>/N`, assigned
  purely for `state`-variable key uniqueness). `compile_sub_body_with_deprecation`
  built the sub's `compiled_fns` key — and its `CompiledFunction::package`
  field — directly from that synthetic name.
- At runtime, a block never pushes its own synthetic package as
  `current_package()` (`call_compiled_closure_with_topic`'s package guard
  explicitly skips any package containing `::&`, by design — a bare
  block/closure is not itself a package boundary). So `foo()`'s call site
  resolved `current_package()` to the REAL enclosing package (e.g. `GLOBAL`),
  and `find_compiled_function`'s runtime lookup — which reconstructs
  candidate keys from `Interpreter::bare_name_packages()` — could never
  reconstruct a key containing the closure's synthetic name. The lookup
  missed on every call, no matter how many times the block ran.

Confirmed with `rust-gdb` breakpoints (per the project's debugging
guidelines): `Interpreter::compile_and_call_function_def` fired once per
loop iteration for the buggy shape and never fired at all for the
equivalent tree-walk-carrier (`lives-ok { ... }`) invocation, which compiles
its own body (and the nested sub) into one dedicated chunk from the start.

## Fix

`compiler::Compiler::runtime_current_package()` (new, in `src/compiler/mod.rs`)
resolves the REAL package a `RegisterDecl`/`RegisterSub` op compiled from
the current code will actually see as `current_package()` at runtime: when
`current_package` contains the `::&` synthetic marker, it falls back to
`enclosing_package` (captured before the state-scope override, and
propagated unchanged through arbitrarily deep closure/sub nesting) — the
exact same rule `qualified_class_decl_name`/`qualified_role_decl_name`
(`src/compiler/helpers_method_body.rs`) already used for class/role
declarations inside a block (ADR-0019 D3-8d). Those two methods were
refactored to share the new helper, and `compile_sub_body_with_deprecation`
now builds a nested sub's `compiled_fns` key (and its `CompiledFunction::package`
field) from `runtime_current_package()` instead of the raw synthetic
`current_package`, matching what the runtime will actually probe for.

This is a plumbing-only fix — no runtime dispatch logic changed. Because the
lookup now succeeds, the closure fast path handles both the plain and
`state`-declaring shapes; the `state` shape never even reaches the OTF gate
that was rejecting it, so nothing about `def_is_otf_compilable_module_single`'s
`declares_state` exclusion (which protects a MODULE sub's shared `state`
cell across threads) needed to change.

## Verification

`MUTSU_VM_STATS=1` on both repro shapes went from "misses every call" to
zero interpreter fallbacks:

- `sub foo() { 42 }` inside the block: `interpreter_fallbacks=0` (unchanged —
  it was already 0 before, since the ladder still eventually reused the
  cached `FunctionDef::compiled`, just after redundant per-call resolution).
- `sub foo() {$ = 42}` (a `state`-declaring nested sub) inside the block:
  `interpreter_fallbacks` dropped from 2,000,000 (50% of opcodes) to 0.

`rust-gdb -batch -ex 'break mutsu::runtime::Interpreter::compile_and_call_function_def' -ex run`
against a 3-iteration repro: the breakpoint no longer fires at all (down
from once per iteration).

Release-build wall-clock, 2,000,000-iteration loop calling the nested sub
(measured on this session's hardware — see `docs/adr` bench-CI note for why
these are dev-machine numbers, not the tracked bench-history series):

| shape | invoked via | before | after |
|---|---|---|---|
| `sub foo() { 42 }` in block | `.()` (`call_compiled_closure`) | 12.40s (main @ 991b55ffa) | **2.19s** |
| `sub foo() { 42 }` at file scope | `.()` | 3.83s (main @ 991b55ffa) | 2.19s (this session's baseline) |
| `sub foo() { 42 }` in block | tree-walk carrier (`lives-ok`) | ~2.00s | 2.10s (no regression) |
| `sub foo() {$ = 42}` in block | `.()` | 14.81s (main @ 991b55ffa) | **4.69s** |
| `sub foo() {$ = 42}` in block | tree-walk carrier (`lives-ok`) | ~5.04s | 4.73s (no regression) |

The `.()` path now tracks the file-scope-sub baseline and the tree-walk
carrier essentially exactly, instead of trailing by 3.2x-3.15x.

Pinned by `tests/nested_sub_in_block_no_otf_recompile.rs` (a `MUTSU_VM_STATS`
integration test, mirroring `tests/proto_method_body_compiled_once.rs`'s
pattern) and `t/nested-sub-in-block-otf-compile.t` (behavioral coverage:
repeated calls, `state` accumulation, re-invocation, parameterized nested
subs, and two block literals with same-named nested subs staying
independent).

## A latent type-check bug this fix exposed (also fixed here)

Landing this fix routes block-nested-sub calls through the light/positional-
light call fast paths (`vm_call_light.rs`, `vm_call_light_typed.rs`) for the
first time, which turned up a genuinely pre-existing bug those paths never
exercised for this shape before: `Interpreter::fast_type_check` matched a
bare (smiley-less) type constraint like `Int` only against a DEFINED value
of the concrete Rust variant (`ValueView::Int`) — never a `ValueView::Package`
type object. A bare `Int $a` parameter means `Int:_` in Raku (accepting both
`Int:D` and `Int:U`), so `sub a(Int $a) { $a }; a Int` incorrectly threw
`Type check failed in binding $a: expected Int, got Package` instead of
accepting the type object. This is exactly what CI's `make roast` caught on
`roast/S06-parameters/smiley.t` (`is { sub a(Int $a) { $a }; a Int }(), Int`)
— confirmed reproducing on `main` too, for the equivalent FILE-SCOPE shape
(`sub a(Int $a) { $a }` at top level already reached the light-call path);
it was invisible for a block-nested sub only because that shape never
reached the fast path until this PR.

Fixed by making `fast_type_check` also accept a `Package` type-object value
whose name matches the constraint, gated to the type names it actually
special-cases (`Int`/`Str`/`Num`/`Bool`/`Rat`) — `Any`/`Mu`/`Cool` keep their
own unconditional `=> true` arm, since they must accept a type object of ANY
name, not just their own. This is safe for every caller of
`fast_type_check`: `is_fast_type_name` (the sole eligibility gate onto the
light/positional-light fast paths, `vm_call_eligibility.rs`) rejects a
smiley-bearing constraint string like `"Int:D"` outright — it never equals a
bare `"Int"`/`"Str"`/... name — so an explicit `:D`/`:U`/`:_` signature never
reaches `fast_type_check` at all and keeps going through the full
`bind_function_args_values` path, which already enforces the smiley
correctly. Pinned by `t/light-call-bare-type-accepts-type-object.t`.

## Why this mattered beyond the immediate speedup

This was the named prerequisite for
[ADR-0055](../../docs/adr/0055-closure-free-vars-resolve-to-their-own-binding.md)
slice 4, which forks `call_sub_value`'s general dispatch onto
`call_compiled_closure` whenever a closure carries compiled code. Before this
fix, every block that declares a helper `sub` would have moved from the fast
column of the table above to the slow one the moment slice 4 landed — the
same "2.4x regression on `roast/S04-declarations/state.t`" shape that sank an
earlier fork attempt (see `news/2026-08/eval-block-value-recompiles-every-call.md`).
That blocker is now cleared; ADR-0055 slice 4 itself is separate follow-up
work, not attempted here.
