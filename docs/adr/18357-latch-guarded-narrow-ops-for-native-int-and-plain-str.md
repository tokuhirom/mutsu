# ADR-18357: Latch-guarded narrow ops for native-int and plain-Str operations

- **Status**: Accepted (maintainer decision 2026-11-21, on the condition that the D3 generic fall-through and the `t/vm/narrow-op-parity.t` parity test ship with slice 1; proposed 2026-11-20)
- **Issue**: [#18357](https://github.com/tokuhirom/mutsu/issues/18357), a sub-issue of
  [#18244](https://github.com/tokuhirom/mutsu/issues/18244). It serves
  [#16897](https://github.com/tokuhirom/mutsu/issues/16897), MCP::Server::Tool::Web's
  `t/12-extract.rakutest`.
- **Refines**: [ADR-18244](18244-wide-raku-code-floor-staged-structural-plan.md) §O3,
  "specialized ops chosen at compile time". This ADR fixes the contract for O3's first two op
  families. The other O3 families stay under ADR-18244.
- **Keeps**:
  - [ADR-0117](0117-str-methods-and-nqp-ops-share-one-routine.md) and
    [ADR-0118](0118-int-operators-share-one-routine.md): one implementation per primitive.
  - [ADR-8751](8751-imports-bind-lexically-in-the-importing-block.md): operators imported in a
    block apply only in that block.
  - [ADR-0004](0004-jit-strategy.md): the Tier-B JIT compiles the same ops the interpreter runs.

## 1. Context

### 1.1 Where the time goes

#16897's budget (issue comment 7041729184) splits `html-to-text` into two phases:

| phase | mutsu | raku |
| --- | ---: | ---: |
| tokenize | 9.4-10.1 s | 2.2-2.6 s |
| render | 14.1-14.8 s | 4.35-4.72 s |

The tokenizer walks the HTML one character at a time:

```raku
$p++ while $p < $len && is-name-char($html.substr($p, 1));
```

The predicates are small subs built from `eq`, `.ord` and chained integer ranges:

```raku
my sub is-space(Str $ch --> Bool:D) {
    $ch eq ' ' || $ch eq "\n" || $ch eq "\t" || $ch eq "\f" || $ch eq "\r";
}
```

### 1.2 Measured cost per shape

Callgrind slopes on a profiling build of main (`bc15b071` plus #18355). Each row is the Ir per
loop iteration, from 20k to 40k iterations:

| shape | mutsu Ir/iter | raku ns/iter |
| --- | ---: | ---: |
| `my int $p; $p++ while $p < $n` (the loop floor) | 1,795 | 32 |
| add `$s.substr($p, 1)` | +2,700 | 130 (total) |
| add `.ord` on that | +1,200 | |
| `bare($c)`, an empty sub with an untyped param | 5,478 | 92 |
| `is-space($c)` | 11,180 | 130 |
| `is-name-char($c)` | 11,852 | 387 |

Two of these costs belong elsewhere:

- The sub call itself (`bare`, 5.5k) is ADR-18244 §O2, lean call frames (#18267).
- Name-keyed state is §O1, static lexical addressing (#18274).

What remains is the cost of the operations inside the loop and the predicates. That is what
this ADR is about.

**The loop floor.** The 1,795 Ir of `$p++ while $p < $n` is eight ops. They split as follows:

- `PostIncrement(0, Some(2))` is ~600 Ir. `exec_scalar_incdec_op` asks whether the name is
  readonly (`check_named_incdec_readonly`, `name_is_readonly_binding_for`, an `Env::get_sym`)
  and clears the slot's metadata lanes. A `my int` local can never be readonly.
- `SinkPop` checks for an unhandled `Failure` on a native int.
- The rest is `NumLt` on two native ints and the dispatch of every op.
- The JIT enters and leaves its range on every iteration (`try_enter_range`, 177 Ir).

**One `eq`.** Inside `is-space`, one `$ch eq ' '` costs ~360 Ir, plus ~100 Ir for each
`GetLocal`/`LoadConst`. With two plain strings, the call chain is:

1. `exec_str_cmp_op`
2. `str_cmp_values`
3. `try_user_str_infix`, which calls `user_infix_override` and through it
   `lexical_operator_in_scope`
4. `str_cmp_values_core`
5. `str_cmp_leaf`, which runs `is_buf_value` twice and `str_context_cow` twice
6. `str_prim::str_eq`

Two plain strings with no user operator in scope need one tag check per operand and the
`str_eq` call.

**The method calls.** `.substr($p, 1)` and `.ord` go through `CallMethodMut` and the method-site
lane. The lane locks the site cache, compares the generation, and rechecks the augment gate.
Only then does it reach the method table's pure row (#18345).

### 1.3 Why the generic op cannot just get faster

Each generic op is generic for a reason that can change at run time:

- A user `infix:<eq>`, `infix:<<>` or `postfix:<++>` candidate can come into scope. It can be
  declared in the unit, exported by a `sub EXPORT` computed at run time, loaded by `require`,
  or bound only inside one block (ADR-8751).
- `Str` can be augmented with a method that shadows `.ord`.
- A scalar can hold a Proxy, a Junction or a mixin.

The compiler cannot prove these absent. What it can prove is narrower:

- The slot is a declared native `int` (`my int $p`; `local_types` knows the constraint).
- The operand is a literal `Str` constant.
- The method name is one of a fixed set of `Str` primitives.

Every other fact is checked when the op runs. A fact that only ever changes one way can be
checked with one word.

The repository already has such a fact: `OPERATOR_IMPORT_POSSIBLE`
(`src/runtime/lexical_operator_import_flag.rs`, #18211). It is a process-wide `AtomicU32`. It is
raised before the first import set that binds an operator is stored, and it is never lowered. A
reader that finds it clear knows that no interpreter in the process has bound such an operator.
The JIT loads it inline for `LexImportBuiltinOp`. This ADR generalizes that pattern.

## 2. Decision

### D1. Narrow ops, emitted only on a static proof

The compiler emits a narrow op in place of a generic op sequence only when the static half of
the proof holds. The first families are:

| family | narrow op | replaces | static proof |
| --- | --- | --- | --- |
| native-int compare | `CmpIntLocals { op, a, b }`, `CmpIntLocalConst { op, slot, const }` | `GetLocal; GetLocal; NumLt` (and the other five orderings) | both slots are declared native `int`, or one is and the other operand is an `Int` literal |
| native-int step | `IncIntLocal { slot, post }`, `DecIntLocal { slot, post }` | `PostIncrement(0, Some(slot))` and the other three forms, plus the `SinkPop` in void context | the slot is a `my int` local: not a parameter, not `is readonly`, never `:=`-bound |
| plain-Str compare | `StrCmpLocalConst { kind, slot, const }`, `StrCmpLocals { kind, a, b }` | `GetLocal; LoadConst; StrEq` (and `ne lt gt le ge`) | one operand is a local slot and the other is a `Str` literal or a local slot |
| Str primitive method | `StrPrimLocal { prim, slot, args }` | `GetLocal; ...; CallMethodMut` for `.ord`, `.chars`, `.substr(Int, Int)`, `.index(Str, Int)` | the method name is one of the listed primitives, with positional arguments only, and the receiver is a local slot |

A narrow op folds its operand loads into itself (it reads its slots directly), so it also
removes the `GetLocal`/`LoadConst` dispatches it replaces.

A narrow form that replaces **one** op one-for-one does not need a new opcode. The step forms
are an example: `PostIncrement(name, Some(slot))` has nothing to fold. The compiler's proof is
then a per-chunk bit per slot, derived lazily from the chunk's final binding descriptors like
`local_read_plain`, and the existing op's arm tests that bit first. This is the same decision as
a narrow opcode, with the same run-time guard (D2) and fall-through (D3). In addition, every
analysis that scans ops by name keeps seeing an op it already understands. A new opcode is for a
narrow form that fuses several ops.

### D2. Run-time guard: operand tags plus latch words

Each time it runs, a narrow op checks two things:

1. **Operand tags.** The slot holds the shape the fast arm handles: a NaN-boxed small int, or a
   plain `Str` with no mixin, Proxy or Junction. This is one tag compare per operand.
2. **Latch words.** One or more process-wide monotone flags are all clear. The flags live in
   one `AtomicU32` bitset, `NARROW_OP_LATCHES` (a new `src/runtime/narrow_op_latch.rs`, next to
   the existing flag):

| bit | raised when | narrow ops it disables |
| --- | --- | --- |
| `INT_CMP_OPERATORS` | any `infix:<< < <= > >= == != >>` candidate is declared, exported or lexically imported anywhere in the process | `CmpInt*` |
| `INT_STEP_OPERATORS` | any `prefix:<++ -->` / `postfix:<++ -->` candidate is declared, exported or imported | `IncIntLocal`, `DecIntLocal` |
| `STR_CMP_OPERATORS` | any `infix:<eq ne lt gt le ge>` candidate is declared, exported or imported | `StrCmp*` |
| `STR_METHODS` | `Str`, `Cool`, `Any` or `Mu` gains or changes a method by `augment`, `^add_method`, a role mixed into the type object, or `^compose` | `StrPrimLocal` |

`OPERATOR_IMPORT_POSSIBLE` raises all three operator bits. A narrow op's latch test is a
single load and mask.

The latch rules are the ones `OPERATOR_IMPORT_POSSIBLE` already follows:

- **Raise-only.** A latch is never lowered. Several interpreters and threads share the process,
  and a raise-only flag cannot be cleared by any ordering of their writes while one of them
  still depends on it.
- **Raised before the state is visible.** A latch is raised before the candidate, import set or
  method is stored, with `SeqCst`. A reader that sees the new state also sees the latch.
- **Over-approximate.** A latch is raised by the event anywhere in the process, not per unit or
  per block. A raised latch only sends the op to the generic handler, and the generic handler
  makes the exact decision: per-unit `user_infix_override`, per-block ADR-8751 scopes, and the
  real method resolution.

### D3. When a guard fails, the op falls through to the generic handler

A narrow op whose tag check or latch check fails runs the **existing generic handler** for the
semantics it replaced, in the same frame and at the same pc. It pushes its operands as the
generic sequence would have, then calls that handler: `exec_str_cmp_op`,
`exec_scalar_incdec_op`, the `CallMethodMut` path.

This means:

- There is no deoptimization of the frame, no recompilation and no on-stack replacement.
- The fall-through is the same code that runs today. It is not a new slow path: no new
  tree-walk or `call_method_with_values` route is added.
- A program that raises a latch pays one load and branch per narrow op, plus today's cost.

Recompiling the affected code when a latch rises was rejected (§3).

### D4. One implementation per primitive

The fast arm of a narrow op calls the same routine that the generic op's fast case calls:

- `str_prim::str_eq` and `str_prim::str_order` for `StrCmp*`;
- the `str_prim` functions behind the method table's pure rows for `StrPrimLocal`;
- `builtins::arith`'s native-int routines for `CmpInt*` and the steps.

It inlines no comparison, wrap or codepoint/grapheme logic of its own. A native `int` increment
wraps exactly as the generic op does. `make check-prims` keeps rejecting a hand-rolled copy in
the new handlers.

### D5. The JIT compiles narrow ops, or the op does not land

A narrow op replaces ops that Tier B already lowers. A stage may therefore only land a narrow op
together with its Tier-B lowering. The lowering follows the `LexImportBuiltinOp` pattern:

- an inline load of `NARROW_OP_LATCHES` and the tag checks, then the fast arm:
  - native-int compare and step lower to inline Cranelift integer instructions on the slot
    word;
  - `StrCmp*` and `StrPrimLocal` lower to a call of a narrow helper that runs the `str_prim`
    routine;
- on a failed check, a call of the generic op's existing helper at the same pc.

Without its lowering, a narrow op would end the JIT range that today covers its generic
sequence. That would trade an interpreter win for a JIT loss. The `jit` feature's off
configuration (`make lint`) still builds, because a narrow op is an ordinary interpreter op
first.

### D6. Testing: every narrow op against its generic op

- **A parity switch.** `MUTSU_NARROW_OPS=0` makes the compiler emit the generic sequence. It is
  read once per process and is for testing and bisection only.
- **A parity test.** `t/vm/narrow-op-parity.t` runs one script under both settings and compares
  the outputs. The script covers:
  - native `int` wrap at both ends;
  - Str values with combining marks, where grapheme and codepoint indexing differ;
  - empty strings;
  - an operand of each shape the tag check rejects: Proxy, Junction, mixin, `IntStr`, Failure.

  It has the shape of `t/vm/nqp-str-prim-parity.t`.
- **One latch test per bit.** Each test raises the latch in one of its ways (a declared
  `infix:<eq>`, an EXPORT of one, a block-scoped `use`, `augment class Str`) and checks that
  every narrow op of that family now gives the user's answer. Each test covers a narrow op that
  was already executed before the latch rose, which proves fall-through without recompilation.

### D7. Order of stages

Each stage is one PR. Each PR carries:

- callgrind slopes and wall times (at least 5 interleaved runs);
- regression and parity tests and a `news/` entry;
- a check of the real MCP::Server::Tool::Web distribution next to raku.

| stage | content | target |
| --- | --- | --- |
| 1 | `NARROW_OP_LATCHES`, `MUTSU_NARROW_OPS`, the parity harness; `CmpInt*`, `IncIntLocal`, `DecIntLocal` and their lowering | `my int $p; $p++ while $p < $n` ≤ 600 Ir/iteration, from 1,795 (ADR-18244 §O3's own target) |
| 2 | `StrCmpLocalConst`, `StrCmpLocals` | `$ch eq ' '` on two plain strings ≤ 120 Ir, from ~360 plus loads; `is-space` −1.5k Ir |
| 3 | `StrPrimLocal` for `.ord`, `.chars`, `.substr(Int, Int)`, `.index(Str, Int)` | `$s.substr($p, 1).ord` ≤ 1.5k Ir, from 3.9k |
| 4 | the flat hot dispatch of ADR-18244 §O3, the hottest ~30 ops in one `match` before the grouped dispatcher | measured on its own; dropped if under 1% |

**Expected gain.** On the tokenizer's per-character work, stages 1-3 remove about 5k of the
~17k Ir per character. On `html-to-text` as a whole that is **~5% (≈1.05x)**, because the
predicates' sub calls and the render phase are O1/O2 territory. Loop benchmarks gain more:
int-arith, bench-string and bench-regex-long-subject. #18357's close condition is the stage
targets above.

## 3. Alternatives considered

- **Make the generic handlers faster** (a fast case inside `str_cmp_values`, as #18351 did for
  plain strings). This is still worth doing where a narrow op cannot be emitted. It keeps the
  dispatch chain, though, and the per-op `user_infix_override` probe, and the operand
  loads. The narrow op is that fast case decided at compile time, behind one latch word instead
  of a lexical-scope probe.
- **Run-time quickening**: rewriting an op in place after observing its operand types.
  - `CompiledCode` is shared across threads behind an `Arc`, so an in-place rewrite needs atomic
    op slots or a copy per thread.
  - The JIT would see a different op stream from the one it compiled.

  A compile-time proof plus a tag check gets the families above without either cost.
  Revisit quickening for operands with no static type (an untyped `$ch`) once stages 1-3 have
  been measured.
- **Recompile when a latch rises.** That needs every live frame of the affected code found and
  moved (OSR) or left on stale code. Falling through on a load and branch costs a few Ir and has
  no such machinery.
- **Per-unit or per-block latches** instead of process-wide ones. These would keep narrow ops
  fast in a program where one unrelated module declares an `infix:<eq>`. But the op would need
  to know its unit and block scope set at run time, which is the probe this ADR removes. The
  generic handler already makes the exact decision, so the over-approximation costs only speed,
  and only in such programs.
- **Native `int` slots kept in registers by the JIT.** This is ADR-18244's rejected-for-now
  register-allocating tier. Narrow int ops are its prerequisite: they give the JIT ops whose
  fast arm is a single integer instruction.

## 4. Consequences

- Programs with no user operators and no augmented `Str`, which is almost all of them, run
  their native-int loops and string predicates without the user-operator probe or the
  readonly/metadata triage.
- A program that raises a latch keeps today's speed, plus one load and branch per narrow op.
  The answers are unchanged.
- Each narrow op is a new opcode. `size_of::<OpCode>()` stays ≤ 48 bytes, so
  `StrPrimLocal`'s arguments live in a constant-pool entry if they do not fit.
- The compiler's `local_types` becomes a proof source, so a bug that records a wrong native
  type for a slot becomes a wrong answer, not just a slower path. The parity test and the
  `MUTSU_NARROW_OPS=0` bisection switch are what catch it.

## 5. Open questions

- **JIT range entry.** The JIT enters and leaves its range once per loop iteration (177 Ir in the
  loop floor). Whether a whole `WhileLoop` whose body is all narrow ops can run as one range is
  a JIT question for stage 1's measurement, not a narrow-op decision.
- **Untyped operands.** `is-space(Str $ch)` has a typed parameter. A `Str`-typed parameter is
  not a proof that the value is a plain `Str`, because an `IntStr` or a mixin also passes the
  check. The tag check covers that case. An untyped `$ch` gets the same narrow op, because the
  guard is the tag check, not the declared type. Whether to emit narrow ops for every
  local-vs-literal `eq` regardless of declared type is decided by stage 2's measurement.
- **Native `num`** (`CmpNumLocals`) waits for a workload that needs it.
