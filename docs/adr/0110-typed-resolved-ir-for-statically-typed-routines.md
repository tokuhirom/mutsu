# ADR-0110: Statically typed routines compile to a typed, resolved IR — on the existing stack machine, not a register-machine rewrite

- Status: Accepted (2026-09-20, approved by tokuhirom; Stage 1 landed 2026-09-20 — see "Implementation status")
- Date: 2026-09-20
- Deciders: tokuhirom, Claude
- Tracked by: [#8895](https://github.com/tokuhirom/mutsu/issues/8895)
- Related:
  [ADR-0004](0004-jit-strategy.md) (Cranelift JIT; tier B is the consumer in Stage 3),
  [ADR-0006](0006-baseline-interpreter-optimizations.md) (classical interpreter optimizations),
  [ADR-0018](0018-slot-addressed-lexical-capture-and-env-sync.md) and
  [ADR-0024](0024-mainline-lexicals-for-named-subs.md) (slot-addressed capture, unit-lexical cells),
  [ADR-0042](0042-type-constraints-belong-to-the-container-not-to-a-name.md) and
  [ADR-0097](0097-a-binding-descriptor-addressed-by-slot.md) (metadata belongs to the binding, addressed by slot — this ADR depends on that principle and extends `BindingDesc`),
  [ADR-0066](0066-call-dispatch-inline-cache.md),
  [ADR-0077](0077-locals-are-a-window-into-a-contiguous-stack.md) (locals are a stack window — the frame this ADR's direct call uses),
  [ADR-0109](0109-native-is-rw-scalar-parameter-reaches-the-light-call-fast-paths.md) (native `is rw` on the light path — superseded *for eligible routines* by §3.3, otherwise untouched),
  [docs/vm-single-store.md](../vm-single-store.md), [docs/env-locals-coherence.md](../env-locals-coherence.md)
- Supersedes the plan of record in: [#8830](https://github.com/tokuhirom/mutsu/issues/8830), [#8831](https://github.com/tokuhirom/mutsu/issues/8831), [#8880](https://github.com/tokuhirom/mutsu/issues/8880), [#8888](https://github.com/tokuhirom/mutsu/issues/8888) **for the JSON::Fast goal**. Those issues stay valid for their own workloads.

## 1. Context

### 1.1 The goal, and the required multiple

`JSON::Fast` is a third-party module, but `zef`/`mzef` reads every `META6.json` through it, so its speed *is* the package manager's perceived speed. The goal is **`from-json` and `to-json` at rakudo parity or better**.

Measured 2026-09-20, `main` at `6a97dc359`, release build, warm precomp cache, timed region = one `from-json` call on a 191,310-byte / 727-record SPDX-shaped document (script in §8):

| | time |
|---|---:|
| rakudo 2026.x | **0.015 s** |
| mutsu | **1.04 s** |

**The required multiple is ~70x.** Per `.agents/skills/perf-tuning/SKILL.md` §5a, every stage below states the multiple it must deliver, and a stage that cannot show its multiple on the gate benchmark is a falsification of this ADR, not a reason to slice finer.

### 1.2 What the previous campaign established

Eight merged perf PRs moved `JSON::Fast` from ~63x to ~59x (`news/2026-09/a-session-of-slices-that-went-nowhere.md`). Their analyses concluded, correctly, that there is no hot spot: `@a.elems` is fifteen layers of 100-900 instructions, each already carrying a cache (#8888); a user method call is a chain of 32 by-name probes (#8880). The proposed remedy was a per-call-site inline cache, and the standing belief was that beating rakudo on `nqp::`-style code ultimately requires a register machine (#8831, which itself noted this "reaches for ADR-0001's rejected level 2").

This ADR rejects both halves of that as the route to the JSON::Fast goal, on the measurements below.

### 1.3 New measurement: the opcode *count* is fine; the cost *per opcode* is the 70x

`MUTSU_VM_STATS=1` (debug build), differencing 1 vs 3 warm-up `from-json` calls on the 100-record document, gives the exact opcode stream of **one** `from-json`:

- **677,883 opcodes and 8,626 `CallFunc`** per 100 records → ~4.93 M opcodes and ~62,700 calls for the 727-record document.
- 1.04 s / 4.93 M = **~211 ns per opcode** (roughly 1,500+ instructions each). rakudo's 0.015 s is **~3 ns per mutsu-opcode-equivalent**.

So the compiler already emits about the right *number* of operations. A switch-dispatched interpreter executing *typed, pre-resolved* operations costs 2-10 ns per simple op (JVM bytecode and WebAssembly are typed stack machines; wasm interpreters such as wasm3/wasmi sit in that range). **The whole gap is that mutsu's bytecode is untyped and name-based, so every opcode re-discovers at run time what the compiler knew statically.**

Per-opcode share of one `from-json` (two-call difference = 1,355,766 opcodes):

| opcode(s) | share | what it is |
|---|---:|---|
| `NqpOp` | 18.3% | boxed `Value` in, boxed `Value` out, id-indexed table |
| `GetLocal` | 12.7% | 232+ instruction guard preamble (#8332) |
| `SetVarDynamic`, `MarkVarDeclContext`, `SetVarTypeScoped`, `SetVarType`, `SetVarTypeHoisted`, `TypeCheck`, `ContainerizePair`, `WrapVarRef`, `TagContainerRef` | **19.1%** | **declaration/transport metadata executed at run time, by name** |
| `GetGlobal` + `SetGlobal` | **10.2%** | **lexicals resolved by name** (module-level `my $ws`, captured outers) |
| `LoadConst`, `Pop`, `Dup`, `SinkPop`, jumps | ~37% | stack traffic and control flow |
| `CallFunc` | 1.3% of opcodes, **~30% of time** | by-name call through the general binder |

### 1.4 What the bytecode looks like

`--dump-bytecode` of `JSON::Fast`'s own `nom-ws` and a caller (repro in §8):

```
== sub nom-ws (str $text, int $pos is rw --> Nil) ==
 0: GetGlobal("ws")            ; a file-scoped `my $ws` — looked up BY NAME, per loop iteration
 1: GetLocal(0)
 2: GetLocal(1)
 3: NqpOp { id: 123, arity: 2 }   ; ordat   — boxed operands, boxed result
 4: NqpOp { id: 7,   arity: 2 }   ; atpos_i
 5: JumpIfFalse(9)
 6: PreIncrement(1, Some("pos"))
 7: Pop
 8: Jump(0)

== in the caller, for `my int $ordinal = nqp::ordat($text,$pos)` ==
 9: SetVarDynamic { name_idx: "ordinal", dynamic: false }   ; env write, by name, EVERY call
10: SetVarTypeScoped { name_idx: "ordinal", tc_idx: "int" } ; the type travels as the STRING "int"
13: NqpOp { id: 123, arity: 2 }
14: TypeCheck("int", Some("$ordinal"))                      ; re-proves what the compiler knew
15: SetLocalDecl { slot: 2, explicit_init: true }

== and for the call `nom-ws($text, $pos)` ==
 1: GetLocal(0); ContainerizePair; WrapVarRef { "text", slot 0 }   ; wrapped although `$text` is NOT rw —
 4: GetLocal(1); ContainerizePair; WrapVarRef { "pos",  slot 1 }   ; the compiler does not know the callee
 7: CallFunc { name_idx: "nom-ws", arity: 2, ... }                 ; callee resolved BY NAME
```

`nqp::while` itself lowers correctly to `Jump`/`JumpIfFalse`. Nothing here is a missing fast path; it is the *design* of the IR: **types are strings, variables are names, callees are names, and rw-ness is unknown at the call site.**

Micro-measurements of that design (release, ns per call, loop skeleton subtracted; script in §8):

| callee shape | mutsu | rakudo |
|---|---:|---:|
| `sub($text, $pos) { $pos + 1 }` (untyped, light path) | 1,016 | 400 |
| `sub(str $text, int $pos is rw)`, **no free variable** | 2,213 | 203 |
| same, body reads the file-scoped `my $ws` (= the real `nom-ws`) | **7,437** | 204 |
| one `nqp::while` iteration inside it (`ordat`+`atpos_i`+`++$pos`) | ~600 | ~21 |

`callgrind` on the `nom-ws` loop: **~85,700 instructions per iteration**, flat profile (`LocalKey::with` 5.8%, `_int_free` 5.5%, `bind_function_args_values_inner` 3.5%, `hash_one`, `unit_lexical_slot`, ...). A single free variable costs ~5,000 ns per call. This is the diffuse "fifteen cached layers" profile again, and no ordering of layer fixes reaches 40 ns.

### 1.5 Why MoarVM is fast here — and it is not the registers

What makes `nqp::add_i($pos, 1)` on a `my int $pos` cost a few cycles on MoarVM is three *static* properties, none of which is "register machine":

1. **Lexical addressing.** Every variable is `(frame depth, slot index)` at compile time. There is no name at run time.
2. **Typed operations and typed storage.** `_i`/`_n`/`_s`/`_o` ops read and write unboxed `int64`/`double`/string registers whose kind is fixed at compile time. No tag test, no boxing, no refcount.
3. **Static call sites.** The callee of a lexical sub call and the callsite shape are fixed; a native `is rw` argument is a typed lexical *reference* (`getlexref_i`), not a wrapper object.

Register-vs-stack is secondary (classically 1.2-1.5x in interpreters), and irrelevant once a JIT runs, because **Cranelift performs register allocation**: wasmtime lowers a *typed stack* bytecode to Cranelift SSA directly. mutsu already ships Cranelift (tier B). What tier B lacks is typed, resolved input — its own module doc records that it must reload the stack pointer at every opcode because values live in a `Vec<Value>` any helper may reallocate.

## 2. Decision

**A routine whose variables, types and callees are statically known is compiled to a typed, resolved instruction set ("TRIR") executed by the same VM, on the same operand stack. We do not rewrite the VM as a register machine.** TRIR has four pillars; they are one design and ship together per routine (§4), because each alone leaves the other three costs in place — which is exactly why #8829/#8868/#8876 moved a numeric loop by 40% and `JSON::Fast` by 0%.

TRIR is **not a second VM**: it is a resolution pass in `src/compiler/`, new opcodes in `src/opcode.rs`, and new arms in the existing `exec_one` dispatch. The existing untyped opcodes remain as the path for everything not eligible.

### 3.1 Pillar 1 — lexicals are resolved at compile time; declaration metadata is static

- Every reference to a lexical of an eligible routine compiles to a slot access: own locals (already slots), **captured outer lexicals and compunit-level `my` variables as a pre-resolved cell handle** stored in the `CompiledCode`/closure at link/closure-creation time (extend ADR-0018/0024's cells; the point is that the *lookup* happens once, not per access). `GetGlobal("ws")`/`SetGlobal` are not emitted for a resolvable lexical.
- A binding's declaration-settled facts — declared type (`int`/`num`/`str`/boxed nominal), `is dynamic`, rw/readonly, native-ness — become fields of `BindingDesc` (`src/binding_desc.rs`, ADR-0097). `SetVarDynamic`, `SetVarType*`, `MarkVarDeclContext` and the post-store `TypeCheck` are **not emitted** for such a binding. Introspection (`$x ~~ int`, `.VAR`, `.WHAT` of a native) reads the descriptor. (#8686 found `t/nativecall/native-value-smartmatch.t` depends on per-variable env metadata; the descriptor is the replacement source of truth for slots that have one.)
- This is the same direction as the single-store work; it does not add a store.

### 3.2 Pillar 2 — native slots and typed opcodes, on a typed operand stack

- `my int`/`my num`/`my str` locals and parameters of an eligible routine live in **typed slots**: the 8-byte slot holds a raw `i64`, a raw `f64`, or a string handle, *not* a NaN-boxed `Value`. The compiler knows each slot's kind; so does the descriptor.
- The operand stack is shared with the untyped opcodes, but a typed opcode pushes/pops **raw words whose kind the compiler has proven** (the JVM/Wasm model). The compiler guarantees stack-kind consistency at every join point; a debug-build verifier pass over each TRIR chunk enforces it (this is the soundness gate — see §5).
- New opcodes (illustrative, final naming is the implementer's): `GetLocalI/SetLocalI`, `ConstI`, `AddI/SubI/MulI/BitAndI`, `IsEqI/IsNeI/IsLtI/IsLeI/IsGeI` → raw 0/1, `JumpIfFalseI`, `IncI/DecI`, `OrdAt(s,i)->i`, `EqAt(s,s,i)->i`, `CharsS`, `SubstrS`, `ConcatS`, `FindNotCclass`, `IsCclass`, `AtPosI(list_i,i)->i`, `BindPosI`, `PushS`..., plus `BoxI/BoxN/BoxS` and `UnboxI/UnboxN/UnboxS` (checked) at the typed/untyped boundary.
- `nqp::add_i` etc. map 1:1 (`src/compiler/nqp_forms.rs::try_compile_nqp_value_op` is the entry point). **Ordinary Raku operators on statically native operands lower to the same ops**: `$pos + 1` with `$pos` an `int` slot is `AddI` — Raku already guarantees a native `int` container holds a native int, with wrapping semantics (`news/2026-09/native-int-wrapping-stays-in-machine-arithmetic.md`). Type inference is deliberately trivial: slot kinds, literal kinds, and typed-op result kinds. No flow-sensitive inference, no speculation.
- Boxing happens only at a boundary: store to an untyped variable, argument to a boxed parameter, return value, element store into a boxed container.

### 3.3 Pillar 3 — static call linkage for lexical subs

- A call whose callee is a lexically visible, non-`multi`, non-`proto` sub (`my sub`, or a package sub resolved at compile time in the same compunit) compiles to **`CallDirect { code, argc }`** — no name, no dispatch-key construction, no candidate resolution.
- The callee's signature is known at the call site, so **the binder is compiled away**: the caller evaluates each argument directly into the kind the parameter declares and the call writes them into the callee's slot window (ADR-0077). If the caller cannot prove an argument's kind it emits the checked `Unbox*`/coercion op *at the call site* (covering the `Bool`→`int` coercion and the range/NaN rejections of `validate_native_int_assignment` that #8686 found the light path lacks).
- A native `is rw` parameter receives a **slot reference** — (frame base, slot index), MoarVM's `getlexref_i` — and the callee's `GetLocalI/SetLocalI` on that parameter become `GetRefI/SetRefI`. No `ContainerizePair`, no `WrapVarRef`, no allocation. A non-rw argument is never wrapped, because the compiler now knows it is not rw.
- Guards, checked once per call and falling back to the existing `CallFunc` path when set: the callee `Routine` has been `.wrap`ped; the callee is not TRIR-eligible (then `CallDirect` still skips resolution but uses the generic binder).
- `return` inside an eligible routine is a **`Return` opcode**. Today `(return %result)` inside `nqp::stmts` is resolved as a *function named `return`* (`function-full-resolve: return=2812`, `function-fallback: return=1406` per 100 records) — that must not survive.

### 3.4 Pillar 4 — execution tiers

- **Tier 0 (Stage 1-2):** the existing `match` dispatch executes TRIR opcodes. They need no `view_kind`, no `Symbol`, no env, so each arm is a handful of instructions. This alone must carry the routine from ~211 ns/op to single-digit ns/op.
- **Tier 1 (Stage 3):** tier B compiles TRIR chunks. Because slot and stack kinds are static, native slots map to Cranelift `Variable`s (`def_var`/`use_var` gives SSA construction for free), exactly as wasmtime does for Wasm locals/stack. Native values never enter the `Vec<Value>`, so the "reload the stack pointer at every opcode" constraint does not apply to them. This is where register allocation happens — by Cranelift, not by us.

## 4. Eligibility: per routine, all-or-nothing, conservative

A routine is compiled to TRIR iff the compiler can prove **all** of:

1. Its body uses only constructs TRIR supports (start: exactly what `JSON::Fast` uses — see Stage 1/2).
2. No construct in its lexical scope can read or write its lexicals **by name**: no `EVAL`, no `::('...')`/`MY::`/`OUTER::`/`CALLER::`/`LEXICAL::` pseudo-package access, no `callframe`-style introspection (`uses_callframe`). `docs/vm-dual-store.md` records that `EVAL` is a permanent by-name lexical writer; TRIR coexists with it by *never sharing a routine with it*.
3. No typed (native) slot is captured by an inner closure, `start` block, `gather`, or phaser body. (A captured native needs a heap cell; v1 simply declines — a later stage may add typed cells.)
4. Parameters are positional scalars with native or plain nominal types, optionally `is rw`/`is copy`, with constant defaults. Anything else (slurpies, named params, `where`, coercion types, sub-signatures) declines.

**A declined routine takes today's path, which is correct.** That asymmetry is the safety argument: CLAUDE.md's definition of risk is an optimization "correct only under an incomplete static analysis". TRIR never relies on proving a *negative about the rest of the program* (e.g. "nobody mutates this variable"); every condition above is a syntactic property of the routine's own lexical scope, and the only cross-routine fact (`.wrap`) is a run-time guard. Mixed frames are normal: a TRIR routine may call an untyped one (box at the boundary) and vice versa (`CallFunc` into a TRIR routine enters through a generic prologue that unboxes/validates the arguments into typed slots).

GC: typed `i64`/`f64` slots are invisible to the collector by construction; a `str` slot holds a refcounted handle and is traced/dropped via the descriptor's slot kind. Frame teardown must drop by slot kind, not assume `Value`.

## 5. Risks and how each is contained

| risk | containment |
|---|---|
| **Two IRs become a second dual mechanism** | Same VM, same stack, same frames; TRIR opcodes are additional `OpCode` variants. Keep `size_of::<OpCode>() <= 48`. Every construct moved to TRIR should, where general, also let the untyped path drop its by-name form (Pillar 1's descriptor fields apply to *all* routines, not only eligible ones). |
| **Raw word misread as a `Value` (memory unsafety)** | Stack-kind verifier on every compiled TRIR chunk in debug builds + `gc-stress`/`jit-stress` CI; typed slots are never reachable through `GetLocal`/env sync (an eligible routine has no env mirror of its typed slots, condition 4.2 makes that sound). |
| **Semantics drift vs the general binder** (`Bool`→`int`, type-object rejection, BigInt range, `--> Nil`, `is copy`) | Differential test: a `t/` file that runs each signature shape through both paths (force-decline via an env var such as `MUTSU_TRIR=off`) and asserts identical results and identical exception types. The env switch is also the A/B measurement tool. |
| **Coverage stalls at "only JSON::Fast"** | Accepted for v1 — the goal is named. But eligibility is general, so any `my int`/`my str`-style routine benefits; Stage 4 widens constructs by ecosystem frequency. |
| **Precomp cache** | TRIR chunks serialize with `CompiledCode`; bump the precomp format version. Remember the warm/cold trap when measuring. |

## 6. Rejected alternatives

- **Register-machine rewrite of the VM.** Pays for the secondary effect. The measured loss is by-name/untyped execution (~211 ns/op), which a register machine with the same untyped, by-name ops would keep. With Cranelift present, register allocation is already solved for the JIT tier. Also collides with ADR-0001's rejected level 2.
- **Per-call-site inline cache as the main lever (#8880/#8888).** Right for dynamic method calls, and still wanted for them. But `JSON::Fast`'s hot calls are *lexical sub calls with static callees*: there is nothing to cache that the compiler does not already know. Calls are ~30% of the run; the other ~70% is ordinary opcodes an IC never touches.
- **Typed `nqp::` ops alone (#8829/#8876 direction).** Measured: op bodies are ~3% of the parse. Without typed slots, static metadata and direct calls, each typed op is still surrounded by boxed loads, by-name stores and declaration opcodes.
- **Continue layer-by-layer slicing.** Falsified by the arithmetic in `a-session-of-slices-that-went-nowhere.md`.
- **A native Rust `from-json`.** Banned by [ADR-0096](0096-batteries-adoption-policy.md); the JSON interception is scheduled for retirement (#8183), and "the real module is slow" is explicitly not a justification.

## 7. Stages, each with a gate that can falsify the ADR

| stage | scope | gate (release, warm, script in §8) | multiple |
|---|---|---|---:|
| **1 — tracer bullet** | Pillars 1-3, tier 0, for the subset needed by `nom-ws`, `parse-thing`, `parse-string` and the micro-benchmarks: native params/locals, `is rw` slot refs, `CallDirect`, `Return`, the int/str ops they use, captured compunit lexical via pre-resolved cell, `nqp::while/if/stmts` | `nom-ws`-shaped call **7,437 ns → ≤ 150 ns**; one `nqp::while` iteration **600 → ≤ 30 ns** | ~50x on the micro |
| **2 — all of JSON::Fast** | every decode and encode routine eligible (hash/array construction ops, `getattr/bindattr`, `p6scalarwithvalue`, `bindkey`, `push`, `str-escape`'s `list_i` ops, dynamic-variable read) | `from-json` 727 records **1.04 s → ≤ 0.045 s (≤ 3x rakudo)**; `to-json` measured and gated the same way; `modules/JSON-Fast` upstream suite green; battery gate green | ~25x |
| **3 — JIT from TRIR** | tier B lowers TRIR chunks with native slots as Cranelift variables | `from-json` **≤ 1x rakudo** | ~3x |
| **4 — widen** | constructs by ecosystem frequency; typed cells for captured natives | per-construct | — |

**Stage 1 is a kill criterion.** If a faithful Stage 1 cannot reach ≤ 300 ns on the `nom-ws` call, stop and record why in this ADR before doing anything else — do not proceed to Stage 2 on hope, and do not substitute finer slices. Stage 1 must land as one coherent PR (or a short sequence on one branch); a Stage 1 that only ships, say, the opcodes without `CallDirect` measures nothing, by §2.

Order of work inside Stage 1 that keeps every commit testable: (a) descriptor fields + stop emitting declaration opcodes for described slots (benefits all code, measurable alone as −19% opcodes on the parse); (b) typed slots + typed ops + verifier; (c) `CallDirect` + slot refs + `Return`; (d) pre-resolved outer cells; (e) measure against the gate.

## 8. Reproduction

Decode benchmark (`tmp/jf-decode.raku`; run twice, quote the second; `raku` runs it unchanged):

```raku
use lib $?FILE.IO.parent(2).add('modules/JSON-Fast/lib').Str;
use JSON::Fast;
my $n = (@*ARGS[0] // 727).Int;
my @recs = (^$n).map: -> $i {
    %( licenseId => "LIC-$i", name => "Some License Name $i",
       reference => "https://example.invalid/licenses/LIC-$i.html",
       isDeprecatedLicenseId => False, referenceNumber => $i,
       seeAlso => ["https://example.invalid/a/$i", "https://example.invalid/b/$i"],
       isOsiApproved => True )
};
my $text = to-json(%( licenseListVersion => "3.20", licenses => @recs ), :!pretty);
from-json($text) for ^((@*ARGS[1] // 1).Int);   # warm-up count; difference two values under MUTSU_VM_STATS=1 for opcodes per call
my $t0 = now; my $r = from-json($text); my $dt = now - $t0;
say "bytes={$text.chars} records={$r<licenses>.elems} from-json={$dt.fmt('%.4f')}s";
```

Call-shape micro-benchmark (the Stage 1 gate):

```raku
use nqp;
my $ws := nqp::list_i;
nqp::bindpos_i($ws, 32, 1); nqp::push_i($ws, 0);
my sub v-orig(str $text, int $pos is rw --> Nil) { nqp::while(nqp::atpos_i($ws, nqp::ordat($text, $pos)), ++$pos); }
my sub v-nofree(str $text, int $pos is rw) { nqp::while(nqp::iseq_i(nqp::ordat($text, $pos), 32), ++$pos); }
my sub v-untyped($text, $pos) { $pos + 1 }
my str $text = "a  b  c  d  e  f  g  h";
my int $N = (@*ARGS[0] // 200000).Int;
my int $i; my int $p; my $t0;
$i = 0; $t0 = now; while $i < $N { $p = 1; $i = $i + 1 }
my $base = (now - $t0) * 1e9 / $N;
$i = 0; $t0 = now; while $i < $N { $p = 1; v-orig($text, $p); $i = $i + 1 }
say sprintf("%-8s %8.1f ns", "orig", (now - $t0) * 1e9 / $N - $base);
$i = 0; $t0 = now; while $i < $N { $p = 1; v-nofree($text, $p); $i = $i + 1 }
say sprintf("%-8s %8.1f ns", "nofree", (now - $t0) * 1e9 / $N - $base);
$i = 0; $t0 = now; while $i < $N { $p = 1; $p = v-untyped($text, $p); $i = $i + 1 }
say sprintf("%-8s %8.1f ns", "untyped", (now - $t0) * 1e9 / $N - $base);
```

Bytecode: `target/release/mutsu --dump-bytecode <file>`. Opcode histogram: `MUTSU_VM_STATS=1 target/debug/mutsu <file>` (the `opcodes executed` line). Read `.agents/skills/perf-tuning/SKILL.md` first — above all the warm/cold precomp trap. Local numbers gate the stages; document-facing numbers come from the bench CI.

## 9. Code map for the implementer

| concern | where |
|---|---|
| per-slot descriptor to extend | `src/binding_desc.rs`, `CompiledCode::binding_descs` in `src/opcode.rs` |
| declaration opcodes to stop emitting | emitted in `src/compiler/stmt.rs`, `expr_block.rs`, `helpers_block_inline.rs`, `mod.rs`; executed via `src/runtime/runtime_var_meta.rs` |
| `nqp::` lowering | `src/compiler/nqp_forms.rs` (`try_compile_nqp_value_op`), `src/vm/vm_call_nqp.rs`, `src/runtime/nqp_ops*` |
| call-argument wrapping to remove | `src/compiler/expr_call.rs`, `src/compiler/helpers_call_args.rs` (`WrapVarRef`/`ContainerizePair`) |
| existing fast call paths and their gate (read, do not extend) | `src/vm/vm_call_light.rs`, `vm_call_light_typed.rs`, `vm_call_eligibility.rs`, `FastParamType` in `src/opcode.rs` |
| by-name free-variable resolution to bypass | `src/vm/vm_env_helpers.rs` (`unit_lexical_slot`, `active_unit_lexical_bucket`), ADR-0024 cells |
| frame window | ADR-0077, `src/vm/vm_env_helpers.rs` |
| dispatch loop | `src/vm/vm_exec_dispatch.rs`, `src/vm/vm_run_loop.rs` |
| JIT (Stage 3) | `src/vm/vm_jit_tier_b.rs` (`emit_int_num_arith` is the template), `vm_jit_support.rs::step_supported`, `vm_jit_compile.rs`, `vm_jit_layout.rs` |
| module under test | `modules/JSON-Fast/lib/JSON/Fast.pm6` (decode: lines ~423-1050), `benchmarks/bench-json-fast.raku` |

## Implementation status

### Stage 1 — landed 2026-09-20

**Result: the iteration gate is met; the call gate is missed at 1.31x rakudo where it asked for 0.74x. The kill criterion is not triggered.**

§7's gates are absolutes taken on a box whose rakudo measured 204 ns on the call benchmark and ~21 ns per iteration. This box's rakudo measures 237 ns and 14.2 ns on the same scripts, so the gates are restated here as ratios to a rakudo run taken in the same session, which is what `.agents/skills/perf-tuning/SKILL.md` §6 requires of any cross-box figure. Release build, warm precomp cache, loop skeleton subtracted, medians of 7 runs each, `MUTSU_TRIR=off` as the A/B control.

| | TRIR off | TRIR on | rakudo (same box) | speedup | vs rakudo | gate |
|---|---:|---:|---:|---:|---:|---|
| `nom-ws`-shaped call, free variable | 9,025 ns | 310 ns | 237 ns | 29x | **1.31x** | ≤ 150 ns there = 0.74x — **missed** |
| same, no free variable | 2,104 ns | 297 ns | 250 ns | 7.1x | 1.19x | — |
| one `nqp::while` iteration | 812 ns | 17.7 ns | 14.2 ns | 46x | **1.25x** | ≤ 30 ns there = 1.43x — **met** |

The kill criterion (§7: stop if a faithful Stage 1 cannot reach ≤ 300 ns on the call, = 1.47x rakudo there) is not triggered at 1.31x.

**Where the missing ~1.8x is.** Not in the typed body: the loop iteration is 46x faster, and at 17.7 ns a three-iteration `nom-ws` body is ~50 ns of the 310. The remainder is the fixed per-call cost that Stage 1 left in place — the `CompiledFns` probe and fingerprint check that re-establish the callee, seeding and tearing down the frame buffers, and reading the `is rw` argument through its container and writing it back at the end. Stage 2 has to replace the last of those anyway: once a TRIR body may call another TRIR routine, copy-in/copy-out stops being sound and the native `is rw` parameter becomes a real slot reference into a shared native frame (§3.3's `getlexref_i`), which removes the container round-trip rather than optimizing it.

`JSON::Fast` itself is unmoved, as expected: every one of its routines still declines, `nom-ws` included, because the real one ends by calling `nom-comment($text, ++$pos)` and Stage 1 admits no call inside a TRIR body. That is Stage 2's subject.

**Deviation from §3.2, recorded deliberately.** The ADR describes native operands as raw words sharing the untyped operand stack, with a debug-build stack-kind verifier as the soundness gate (§5's first risk row). The implementation keeps the typing and drops the sharing: native `int`/`num` operands live in their own `Vec<i64>` bank (`src/trir/exec.rs`), boxed ones in the interpreter's existing `Value` frame and stack. No raw word is ever stored where a `Value` lives, so "a raw word misread as a `Value`" is removed structurally rather than contained by a check, and GC and frame teardown need no change at all. Tier B (Stage 3) is unaffected: a bank whose element kind is static is exactly what Cranelift wants for `def_var`/`use_var`.

**Deviation from §2, recorded deliberately.** TRIR executes in its own small loop (`src/trir/exec.rs`) rather than as new arms in `exec_one`. The concern §2 names — "TRIR is not a second VM" — is met by the eligibility gate instead: the loop executes only operations whose operand kinds the compiler proved, and the compiler declines the whole routine at the first construct it cannot prove, so there is no fallback arm and no second copy of any Raku semantic. Sharing `exec_one` would have added its per-instruction bookkeeping (the `current_code` store, the stats/trace probes, the `pending_where_exception` check, the backtrace check, the poll and halt tests) to operations whose whole budget is a handful of instructions.

**One bug worth recording**, because it is the general hazard in collapsing a sequence of opcodes into one: `CallTrir` initially dropped the argument-source table its `CallFunc` carried, and the post-compile analysis reads exactly that table to learn that a local reaches a call and may be written back through an `is rw` parameter. Without it, a closure over such a variable was vouched for as by-value-capturable and reported the pre-call value. The opcodes a call site emits are inputs to compile-time analyses, not only instructions. The differential test (§5) caught it on its first run.

### Stage 2 — landed 2026-09-20

**Result: the gate is missed by a wide margin, and the measurement falsifies §1.3's causal model. The pillars work; the thing they remove is not what this workload pays for.**

> **Correction, 2026-09-22 (#9072).** The claim below that the whole hot decode "run[s] as TRIR" was not true for the vendored module. Every statically linked call inside a module bailed: `CallTr` looked the callee up in the running routine's own nested-sub table, and a linked callee's `module { }`-block `my` (`$ws`) was not found. Each bail re-ran the routine on the untyped path. On the SPDX benchmark, entries from the VM completed 0 times out of 1,299, and the profiler's exact routine entry counts were identical with TRIR on and off. Only `parse-string`, which makes no linked call, ever finished. With linkage fixed, a third defect showed itself as wrong answers: an expression-position `return` escaped the linked frame. So the Stage 2 numbers below measure mostly the codepoint memo and a partial TRIR run that was then thrown away, not TRIR running JSON::Fast. After #9072 every entry completes (`MUTSU_VM_STATS`: `trir: entries=811 completed=811 bails=0`), and the 727-record `from-json` goes from 3.2s to 1.9s on a 4-core box, where rakudo takes 0.065s (`news/2026-09/trir-runs-inside-modules.md`). The conclusions about where the rest of the time goes still stand. The text below is left as written.

Stage 2 does what §7 asked of it. A TRIR body may now call out, resolved (`CallTr`, signature known at the call site, binder compiled away, a native `is rw` parameter passed as the absolute index of the caller's slot) or generic (`CallGen`, arguments boxed, ordinary dispatch — what `nom-ws` needs to reach `nom-comment`, declared eleven lines later). Frames nest in one contiguous pair of native/boxed stacks with the boxed halves visited as GC roots; container declarations, `nqp::ifnull`, list literals, string concatenation, hash and array construction, dynamic-variable reads and branch-arm unification compile. In `JSON::Fast` that means `nom-ws`, `parse-thing`, `parse-obj`, `parse-array`, `parse-string`, `parse-true` and `parse-false` — the whole hot decode — run as TRIR, with only `parse-numeric` still declining on the hot path.

Stage 2 IS faster, and nowhere near the gate. Release build, warm precomp cache, medians of 7 runs each, rakudo taken in the same session (`.agents/skills/perf-tuning/SKILL.md` §6):

| | `from-json`, 727 records | gate |
|---|---:|---|
| `MUTSU_TRIR=off` | 1.3834 s | — |
| TRIR on | **1.0820 s** | ≤ 0.045 s — **missed**, at 38x rakudo |
| rakudo, same session | 0.0283 s | — |

A real 1.28x where the gate asked for ~24x more. Where it came from is what matters, and instruction counts are exact where wall clock is noisy: callgrind, differencing two runs that differ only in how many times `from-json` runs (1 vs 10) so startup and module compilation cancel, 200-record document, `--profile profiling`:

| | marginal Ir per decode |
|---|---:|
| `MUTSU_TRIR=off` | 2,715,806,457 |
| TRIR on | 2,456,666,797 |

9.5% fewer instructions for 22% less time — so what TRIR removed costs more than an average instruction, and it is not what the ADR predicted. 8.2 of those 9.5 points are `memcpy` (6.09% → 1.65%) and `core::str::count::do_count_chars` (3.02% → ~0): re-walking the document's UTF-8, which the per-frame codepoint memo avoids. Cache traffic over a 191 KB string, which is why it is worth more in wall clock than in issue slots — and a string cache, not the typed opcodes this ADR is about.

**What §1.3 got wrong.** It divided wall clock by opcode count, got ~211 ns per opcode, and read the quotient as the cost of untyped dispatch. Measured directly, with TRIR **off**, `exec_one` plus `exec_one_dispatch` self cost is **4.6% of the decode**. An opcode's cost is overwhelmingly what its *handler* does — allocate, hash a name, intern a symbol, copy a string — and TRIR removes the dispatch, not the handler. Inside `run_trir_chunk`, 96% of the time is in calls back out of it (`exec_trir_inner_call`/`exec_trir_generic_call`, inclusive 1.398 G of 1.459 G on the Stage 2 profile) and the TRIR loop itself is ~1.2% of the program. TRIR made the interpretation of `JSON::Fast`'s control flow nearly free and the decode moved 1.28x, most of that from a string cache that came along for the ride: the control flow was never the cost, so removing it could not be the 24x.

**Where the decode's time is**, marginal, TRIR on, nothing above 4.2% and a very long tail: the allocator ~11.6%, `memcpy`/`memcmp`/`memchr` ~5%, the dispatch loop ~4.6%, hashing and hash-table probes ~3.3%, thread-local access (the `Symbol` interner, `MetaNs`) ~2.8%, NaN-box encode/decode ~3%, run-time type-name resolution ~1.5%. By inclusive cost: `nqp::` op bodies 16.9%, the general binder 16.1%, `Env::get_sym` 9.3%, `Symbol::intern` 4.3%. The allocation count is the standout — **1,628,921 heap allocations per decode of 200 records** (1,659,471 with TRIR off), about 8,100 per JSON record of seven fields and a two-element array.

It is a constant factor, not an asymptotic bug: across 25 KB to 210 KB documents mutsu decodes at a flat 6.4-7.4 µs/byte and rakudo at 0.10-0.18 µs/byte, and the ratio does not move over an eightfold size change, so there is no super-linear term to find and remove.

**Consequences for §7's remaining stages.** Stage 3 lowers TRIR chunks to Cranelift, and those chunks are 96% callouts, so it would compile away the ~1.2% that is left — its `≤ 1x rakudo` gate is unreachable by itself for this workload, on the same measurement. Stage 2's own remaining half (typed forms for the hot `nqp::` string/hash ops, which would remove the boxing and the id-indexed dispatch) is worth up to the 16.9% those bodies cost, not the 24x the gate asks for. **Whether to supersede this ADR is a decision for the maintainer** (CLAUDE.md reserves new and superseding ADRs to them); what the measurement supports is that the 70x gap on `JSON::Fast` is a flat per-primitive cost spread across allocation, name resolution and value representation, and that it has to be attacked there.

The three costs that measurement names are filed on their own, so they survive whatever is decided about this ADR: [#8898](https://github.com/tokuhirom/mutsu/issues/8898) (1.63 M heap allocations per decode), [#8899](https://github.com/tokuhirom/mutsu/issues/8899) (13.6% in run-time name resolution), [#8900](https://github.com/tokuhirom/mutsu/issues/8900) (`nqp::` op bodies 16.9%, the general binder 16.1%).

**What Stage 2 is still worth.** TRIR is correct (the differential fixture agrees exactly with `MUTSU_TRIR=off` across every shape, including a `die` thrown out of a TRIR frame followed by a successful call through the same routine), it is the substrate any later JIT needs, and its micro-gates from Stage 1 stand. It is kept, default on.

**Four bugs fixed on the way, and one shape they share.** A call site is linked to TRIR only once it has executed, so the FIRST call to a routine runs untyped and every call after it runs typed. A broken TRIR path therefore gives a right answer followed by wrong ones, which reads like flakiness and is not — every regression test added for these calls its routine at least twice.

- **A trailing `if` returned `Nil`.** A routine's final `if`/`elsif`/`else` is its value in Raku; TRIR compiled the branches for effect and emitted `ReturnNil`. A wrong answer, not a missing optimization, so such a routine now declines.
- **A generic call resolved names in the wrong package.** A TRIR frame is not a `RoutineFrame`, so nothing established the routine's own declaring package the way `call_compiled_function_named_inner` does; a body in `module C` resolved its callees against the caller's package and a package-scoped `multi` was not found at all.
- **Every generic-call argument was containerized.** A TRIR body cannot see whether its callee will write an argument, so it passed them all as containers — which is not transparent: a container reaches a `proto`'s `{*}` re-dispatch as itself and fails the winning candidate's own type check ("expected Str, got Str"), and `nativecast`'s type-object check rejects it too. The callee's `is rw` positionals are looked up by name now and only those are containerized.
- **Branch-arm unification shifted jump targets it had not moved** — the general hazard in patching jump targets after the fact. The shift was computed from `then_kind != else_kind`, right when the `then` arm is the native one (the box is *inserted* before its jump) and wrong when the `else` arm is (that box goes on the end and shifts nothing). The caller then looked for its own `Jump` one slot too far along, did not find it, and declined the whole routine, silently. `nqp::if(cond, die-helper(...), $pos)` is exactly that shape, and it is the shape of every `JSON::Fast` scanner's error check.

### Stages 3-4

Not started; see the consequences recorded under Stage 2 before starting either.

**2026-09-23:** those consequences were drawn from a TRIR that bailed on every linked call (see the correction under Stage 2). A replacement plan, re-derived from measurements of a TRIR that completes, is proposed in [ADR-0112](0112-trir-completion-plan-for-beating-rakudo.md).
