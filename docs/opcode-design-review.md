# Opcode / instruction-set design review (2026-07-06)

A design audit of the bytecode instruction set (`src/opcode.rs`, `OpCode`) and
its dispatch (`src/vm/vm_exec_dispatch.rs::exec_one`). This records what was
measured, what was fixed, and what remains as known-but-deferred work, so
future instruction-set decisions start from data instead of re-auditing.

Related: [ADR-0004](adr/0004-jit-strategy.md) (JIT Tier A subroutine-threads
this instruction set — it is an asset to preserve, not replace),
[gc-post-3a-roadmap](gc-post-3a-roadmap.md) §3 (NaN-boxing targets `Value`
size; `OpCode` size is a separate axis, addressed here).

## Snapshot at audit time

- ~340 `OpCode` variants (CLAUDE.md's "~100" was stale). Biggest families:
  calls (27), variables (23+), type checking (21+4), functions (21), set ops
  (18), arithmetic (18).
- Dispatch: one big `match &code.ops[*ip]` (`exec_one`), fetch by reference,
  operands are mostly `u32` constant-pool indices. Compound loop ops
  (`ForLoop`/`WhileLoop`/`CStyleLoop`/`RepeatLoop`) run the whole loop inside a
  single `exec_one` via nested `run_range` mini-loops — no jump round-trip per
  iteration. §1.5 slot-baking (`Option<u32>` compiler-resolved local slots) is
  pervasive and good.
- No comment/doc treats the dispatch `match` itself as a bottleneck; measured
  hot costs are env cloning, call dispatch, and `Value` clone/drop (PLAN §5).

## Findings and status

### 1. `size_of::<OpCode>() == 192` bytes — FIXED (now 48)

A Rust enum pads every variant to the widest one, and `Vec<OpCode>` therefore
paid a 192-byte stride for *every* instruction (`LoadConst`, `Add`, ...). The
sole driver was `ForLoop`, which carried a 21-field spec inline including
3× `Vec<String>`, 1× `Vec<Option<u32>>`, 2× `Option<String>`.

Fix: the spec is now `ForLoopSpec` in `opcode.rs` and the variant is
`ForLoop(Box<ForLoopSpec>)` → `size_of::<OpCode>()` = **48 bytes** (4× smaller
instruction stream). The VM borrows the boxed spec directly, which also
removed the per-loop-entry clone of all heap fields (the old dispatch arm
cloned `label` + 3 Vecs + `single_array_source` into a stack `ForLoopSpec` on
every loop entry).

Guard: `opcode_size_guard` test pins `size_of::<OpCode>() <= 48`. The widest
remaining variants are `SmartMatchExpr` and `Subst` (~48B, each one
`Option<String>` / a few `Option<u32>`s wide). Box any new payload that would
exceed this.

### 2. Heap payloads inline instead of constant-pool indices — PARTLY OPEN

10 variants carried `String`-family payloads inline; `ForLoop` (the worst) is
fixed. Still inline, cloned per execution:

- `Last`/`Next`/`Redo` (`Option<String>` label) — cloned into the control-flow
  signal on every hit (`vm_exec_dispatch.rs` `sig.label = label.clone()`).
- `WhileLoop`/`CStyleLoop`/`RepeatLoop`/`DoBlockExpr` (`Option<String>` label)
  — cloned per loop entry.
- `SmartMatchExpr.lhs_var` (`Option<String>`).

These are per-loop-entry / per-signal (not per-iteration) costs and labels are
rare, so they were left for a follow-up: migrate labels to constant-pool
`Option<u32>` like every other name operand. That would also shrink `OpCode`
below 48.

### 3. Dead variants — FIXED

`IsNil`, `JumpIfNil`, `IndexAutovivify` had live `exec_one` arms but zero
compiler emit sites; all three removed (the `exec_index_autovivify_op` helper
stays — it is reached from `IndexAutovivifyLazy`'s fallback). `MultiDimIndex`
was mis-marked `#[allow(dead_code)]` while actually emitted
(`compiler/expr.rs`); the stale attribute is removed.

### 4. No per-opcode execution profile — FIXED

`MUTSU_VM_STATS` counted call/env/GC events but not opcode frequency, so
fusion/shrinking decisions had no empirical basis. `exec_one` now feeds a
per-opcode histogram (discriminant-keyed; variant name derived once via
`Debug`), dumped as `opcodes executed total=... (top 30): ...`. Zero cost when
stats are off (one cached bool load).

First data point (`my $s = 0; for 1..1000 { $s += $_ }`): one iteration of
`$s += $_` executed 6 ops — `SetSourceLine`, `GetLocal`, `SetLocal`, `Add`,
`CheckReadOnly`, `GetGlobal`. That immediately suggested follow-up targets:
`SetSourceLine` fired per statement even in hot loops, and a compound-assign
to a local still pays a `CheckReadOnly` + env-path `GetGlobal` pair.

`SetSourceLine` is now gone (ADR-0006 §2.3): the source line is static
per-instruction data, so `CompiledCode` carries an ip -> line table
(`op_lines`) and `cur_source_line` is refreshed by `sync_source_line(code, ip)`
in the arms that can *observe* a line (calls/reentry into user code, frame
pushes, declaration registrations, raise sites) plus the JIT call shims. That
removed 11% (time-parts) to 21% (fib) of executed opcodes, for -3.4%
instructions on fib in the interpreter — refreshing on *every* op instead cost
more than the dispatch it saved (+7.8% instructions), which is why the sync is
opcode-selective. **When adding an opcode whose handler can call user code, push
a frame, register a declaration, or raise a warning/error, call
`sync_source_line` at the top of its arm.** `CheckReadOnly` / `GetGlobal` on a
compound-assign remain open.

### 5. Per-instruction dispatch overhead — OPEN (deferred)

Two unconditional costs run before every instruction's match arm:

- `self.current_code = code as *const _ as usize` (`exec_one` prologue) — a
  raw-pointer store per instruction; frame-entry granularity would suffice,
  but the lazy-force reconcile (Slice F) depends on it being exact, so hoisting
  needs its own verification pass.
- `trace_log!("vm", ...)` — `trace::is_enabled` (cached config load + phase
  scan) per instruction even when tracing is off.

Both are small constants but sit on the hottest path in the program. Worth a
measured pass; not blindly.

### 6. Misleading `Jump(i32)` encoding — OPEN (cosmetic)

Jump payloads are `i32` but hold *absolute* instruction indices (applied as
`*ip = target as usize`), not relative offsets. Rename/retype (`u32`) when
next touching the jump family. Also `goto`/label resolution
(`find_label_target`) is a linear scan of `code.ops`; fine at current usage
frequency.

## Design verdict (kept for context)

The instruction set's *shape* is sound for this project's trajectory: constant
-pool operands, compound structured loops, baked local slots, and fused hot
ops (`AtomicCompoundVar`, `ArrayPush`, `JunctionAnyN`) all survive contact
with the JIT plan (Tier A translates opcode sequences into helper calls, so
fewer/fatter semantic ops are an advantage, and the interpreter remains the
bailout tier — interpreter-side wins keep paying off post-JIT). The problems
were operational, not architectural: one variant taxing the whole stream's
memory layout, unmeasured frequency, and accumulated dead weight. Variant
count (~340) is high but mostly reflects Raku's semantic surface plus
slot-baked/fused specializations; consolidation (e.g. `ContainerEq`×4,
`IndexAssign*`×6, `MakeAnonSub`×3) is possible but should be driven by the new
histogram, not aesthetics.
