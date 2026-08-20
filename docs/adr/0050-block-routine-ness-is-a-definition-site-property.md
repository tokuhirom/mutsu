# ADR-0050: A Block's routine-ness is a definition-site lexical property, not a re-derived dynamic one

- Status: Proposed (design complete; implementation not started)
- Date: 2026-08-20
- Origin: `todo/deep/nextsame-in-wrap-closure-lexical-return-target.md` (the
  architectural half; the small half became
  `todo/tickets/nextsame-tail-call-is-not-a-real-return-signal.md`)
- Related: [ADR-0037](0037-eval-context-frame-owns-the-return-target.md) — the
  *other branch of the same `if`*. ADR-0037 fixes the `is_eval_unit == true`
  branch of `compile_block_value_opts`'s `in_routine` derivation; this ADR is
  about the `else` branch, which every re-compiled closure body takes.

## 1. Context

Raku decides where a `return` goes **lexically**: a bare `Block` (`{ ... }`,
`-> |c { ... }`) is not a `Routine` and therefore not a return boundary, so a
`return` written inside one keeps searching lexically outward for an enclosing
`sub`/`method`. With no enclosing Routine at all, it is an error at the
`return` site — `X::ControlFlow::Return`, "Attempt to return outside of any
Routine".

mutsu already implements exactly this, and implements it in the right place —
the **compiler**:

```rust
// src/compiler/stmt.rs:2866-2875
Stmt::Return(expr) => {
    self.with_escape(true, |c| c.compile_expr(expr));
    if self.is_routine {
        self.code.emit(OpCode::Return);
    } else {
        self.code.emit(OpCode::ReturnFromNonRoutine(self.lexically_in_routine));
    }
}
```

`OpCode::ReturnFromNonRoutine(false)` throws `X::ControlFlow::Return` right at
the `return` (`src/vm/vm_exec_dispatch.rs:4100-4114`);
`ReturnFromNonRoutine(true)` raises a `CX::Return` signal that the closure
boundary re-targets at the closure's captured `__mutsu_callable_id`
(`src/runtime/resolution_call_sub.rs:1060-1081`, twin at
`src/vm/vm_closure_dispatch.rs:862-895`), so it unwinds *past* any intervening
routine to the lexically enclosing one. Both flags are computed at the block's
**definition site** by `compile_closure_body_with_routine_flag`
(`src/compiler/helpers_sub_body.rs:1312`), which is handed `is_routine: false`
for a pointy/bare block. That is the correct, lexical answer.

### 1.1 The defect — the definition-site answer is thrown away on re-compile

A closure body is compiled twice. The definition-site compile above produces
the correct classification. But when the body is later re-compiled by the
block-value carrier — `call_sub_value` → `eval_block_value_inner` →
`compile_block_value_cached` → `compile_block_value_opts`
(`src/runtime/resolution_eval.rs:128-134`) — the classification is
**re-derived from the dynamic call stack**:

```rust
let in_routine = if is_eval_unit {
    self.enclosing_routine_exists()
} else {
    !self.routine_stack.is_empty()
};
compiler.is_routine        = in_routine;
compiler.lexically_in_routine = in_routine;
```

For any re-compiled closure body the `else` branch runs, and
`!self.routine_stack.is_empty()` is a question about *who is on the stack right
now*, not about what encloses the block in the source. A bare block invoked
from inside any routine therefore compiles as `is_routine = true` — i.e. as its
own return boundary — and `OpCode::Return` is emitted unconditionally,
erasing both halves of the lexical answer.

The comment at that site already records the bind:

> Only for an EVAL unit. The other callers hand this an ordinary closure/sub
> BODY to (re-)compile, and there the live frame is the routine being run —
> including an anonymous `sub`, which pushes a block frame — so narrowing it
> would turn their `return` into a throw.

That is true of the *predicate*, and it is why this cannot be fixed by
narrowing the predicate: the predicate is answering the wrong question
entirely. The block already knows its own answer; the carrier just fails to
carry it.

### 1.2 Measured, against `raku` v2026.06 (mutsu @ `227e38e4f`)

The visible symptom is a `.wrap()` wrapper block, because `.wrap()` routes the
wrapper through `call_sub_value` (`src/vm/vm_call_method_compiled.rs:333` →
`check_method_wrap_chain` → `vm_call_sub_value`), which is precisely the
re-compiling carrier.

**(a) Wrapper block with no lexically enclosing Routine — raku dies, mutsu
returns.** (`tmp/v13`)

```raku
class C { method m() { say "orig"; "o" } }
my &w = -> |c { say "wrap"; return "R" };
C.^lookup('m').wrap(&w);
say C.new.m;
say "after";
```

```
raku                                        mutsu
wrap                                        wrap
Attempt to return outside of any Routine    R
  in block <unit> at ... line 2             after
```

Confirmed under `rust-gdb -batch`: the **definition-site** compile of that
block reaches `compiler/stmt.rs:2869` with `self.is_routine == false` and
`self.lexically_in_routine == false` (frame `compile_closure_body_with_routine_flag
(..., is_routine=false)`) — the correct answer. A **second** compile of the same
body then reaches the same line via `compile_block_value_opts` →
`Compiler::compile` → `compile_unit` with `self.is_routine == true`, and it is
that chunk that runs: the breakpoint on the `ReturnFromNonRoutine` arm
(`vm_exec_dispatch.rs:4101`) never fires, while `RuntimeError::return_signal`
is reached from `vm_exec_dispatch.rs:4098` — the `OpCode::Return` arm.

The same shape with the block called *directly* (not through `.wrap()`) dies
correctly in mutsu, matching raku — the pointy block invoked from a sub
(`tmp/v12`), from a method (`tmp/v15`), and via an intermediate `my &w`
(`tmp/v16`) all agree. Only the re-compiling carrier diverges.

**(b) The lexical-target half is masked, not fixed.** When a Routine *does*
enclose the block, the wrong `is_routine = true` classification is papered over
downstream: the untargeted `CX::Return` reaches
`resolution_call_sub.rs:1060`'s non-routine arm — which keys off `data`'s
own block-ness (`data.is_bare_block || !cc.is_routine`), not the re-compiled
chunk's flag — and is stamped with the captured `__mutsu_callable_id`. So

```raku
sub run5() {
    class C5 { method m() { say "orig"; "o" } }
    C5.^lookup('m').wrap(-> |c { say "wrap"; return "R" });
    say C5.new.m; say "run5-end"; return "run5-normal";
}
say run5();
```

answers `wrap / R` in both — correct, by a different route. The masking is
what makes (a) look like an isolated `.wrap()` quirk instead of a general
classification loss.

## 2. Decision

**A Block's `is_routine` / `lexically_in_routine` classification is fixed at its
definition-site compile and travels with the callable. The block-value carrier
must read it, never re-derive it.**

Concretely:

1. `SubData` / `CompiledCode` records the definition-site pair
   (`is_routine`, `lexically_in_routine`) as computed by
   `compile_closure_body_with_routine_flag`. `CompiledCode::is_routine` already
   exists and is already read at two closure boundaries
   (`vm_closure_dispatch.rs:866`, `resolution_call_sub.rs:1061`) — this
   extends the same recorded fact with its `lexically_in_routine` companion and
   makes it the *only* source for the re-compile.
2. `compile_block_value_opts` takes the classification as an argument rather
   than sampling `self.routine_stack`. Its callers supply it:
   - the closure-body carrier (`eval_block_value_inner` /
     `compile_block_value_cached`) passes the `SubData`'s recorded pair;
   - the EVAL-unit caller keeps its own derivation, which is ADR-0037's
     subject and is unaffected by this ADR;
   - a caller with no owning callable (a genuinely ad-hoc body) keeps today's
     dynamic answer as an explicit, named fallback rather than an implicit one.
3. The classification joins `carrier_compile_ctx_key`
   (`resolution_eval.rs:216`) so the compile cache cannot serve a chunk
   compiled under the other classification. This is the same cache-key
   discipline ADR-0037 §2.3 imposes on its own context field, and for the same
   reason: today the key is derived from ambient state that the classification
   is also derived from, so the two happen to agree; once the classification
   stops being ambient, the key must name it.

The downstream mechanisms need no change. `ReturnFromNonRoutine(true)` already
re-targets correctly, `ReturnFromNonRoutine(false)` already throws with the
right class, and the decline-if-not-my-target logic at every routine boundary
(`vm_call_named_inner.rs:326-343`, `vm_method_dispatch.rs:743-751` / `:1726-1735`,
`resolution_call_sub.rs:1060-1081`) already honours a stamped target. This ADR
only stops the classification being lost on the way to them.

## 3. Alternatives considered and rejected

**(a) Narrow the predicate — replace `!self.routine_stack.is_empty()` with
`enclosing_routine_exists()` for the non-EVAL branch too.** This is what the
existing comment explicitly warns against, and the warning is right: an
anonymous `sub` pushes a *block* frame, so `enclosing_routine_exists()` would
answer `false` for it and turn its perfectly legal `return` into a throw. More
fundamentally it keeps answering a dynamic question in place of a lexical one,
so it would still get (a) wrong whenever some routine happens to be on the
stack. Rejected as the "correct only under an incomplete analysis" shape
CLAUDE.md's gain/risk doctrine names as the *risky* route.

**(b) Do not re-compile closure bodies at all — always run the definition-site
chunk.** Architecturally the cleanest statement, and it would dissolve this ADR
along with several siblings. Rejected as far larger than this decision: the
carrier exists because a closure body's compile depends on ambient context the
definition site does not always have (package scope, `$?DISTRIBUTION`, seeded
sigilless params, prebound placeholders — `resolution_eval.rs:138-155`), and
retiring it is a campaign in its own right. Recording the classification is the
slice that makes *this* fact carrier-independent without taking that on.

**(c) Fix it downstream — teach the `.wrap()` invocation path to throw when the
wrapper block has no captured `__mutsu_callable_id`.** Rejected: it treats the
one visible symptom, leaves every other carrier-invoked block misclassified,
and re-encodes a lexical property as a runtime absence check at a single call
site. It is also demonstrably not where the information is: the compiler had
the right answer and discarded it two frames earlier.

**(d) Do nothing — the enclosing-Routine case already behaves correctly.** It
behaves correctly *by masking* (§1.2(b)): the classification is wrong and a
downstream stamp happens to repair it. That is exactly the band-aid-over-a-
wrong-mechanism shape this repo's doctrine counts as risk, and it leaves the
no-enclosing-Routine case silently wrong.

## 4. Performance

None of this is on a hot path. The classification is two `bool`s recorded on a
structure that already exists, read once per carrier compile — and a carrier
compile is already the expensive path (it compiles an AST). The
`carrier_compile_ctx_key` gains two bits; no additional cache misses are
expected, since the classification is stable per callable and therefore
constant across that callable's cache entries.

## 5. Implementation plan

### Slice 1 — record the definition-site classification

- `compile_closure_body_with_routine_flag`
  (`src/compiler/helpers_sub_body.rs:1312`) already receives the pair; store it
  on the produced `CompiledCode` beside the existing `is_routine`.
- No behaviour change yet. Pin: a Rust unit test asserting the recorded pair
  for a pointy block, a bare block, an anonymous `sub`, and a named `sub`.

### Slice 2 — the carrier reads it instead of sampling the stack

- `compile_block_value_opts` (`src/runtime/resolution_eval.rs:128-134`) takes
  the classification; `eval_block_value_inner` /
  `compile_block_value_cached` thread the owning `SubData`'s recorded pair
  through; the ad-hoc-body fallback is named explicitly.
- Add the classification to `carrier_compile_ctx_key`
  (`resolution_eval.rs:216`).
- Pin: `t/` regression for §1.2(a) (a `.wrap()` wrapper block with no enclosing
  Routine dies with `X::ControlFlow::Return`) plus the four already-correct
  shapes of §1.2 (`tmp/v12`, `tmp/v15`, `tmp/v16`, and the enclosing-Routine
  `run5` case) so the fix cannot regress them.
- Must stay green — this slice changes the answer to a predicate that gates
  `return`'s compilation in every carrier-invoked body, so it is the
  high-blast-radius one: `roast/S04-statements/return.t` (**test 15 is the
  recorded reason `enclosing_routine_exists()` exists at all**,
  `src/runtime/accessors_stack.rs:19-25`), `roast/S06-advanced/return.t`,
  `roast/S06-advanced/wrap.t`, `t/wrap.t` and the wrap/`callsame` corner
  (14 files), plus full CI.

### Slice 3 — residue

- Audit the remaining `compile_block_value_opts` callers for other ambient
  facts re-derived per compile (the `scope` / `enclosing_package` derivation at
  `resolution_eval.rs:140-149` is the obvious neighbour) and record whichever
  of them are lexical properties in the same way, or note explicitly that they
  are genuinely dynamic.
- Retire the origin ticket per the `todo/` lifecycle.

## 6. Out of scope

- `nextsame`/`nextwith`'s tail-call signal not being a real `CX::Return`. That
  is an independent, verified, one-line-per-site defect with its own ticket
  (`todo/tickets/nextsame-tail-call-is-not-a-real-return-signal.md`); it is
  *not* blocked by this ADR and this ADR is not blocked by it. The two were
  filed together only because the same repro exposes both.
- `EVAL ..., context => $frame` and the light-dispatch-path routine frames —
  ADR-0037.
- Deep `CALLER::CALLER::` / `callframes()` chains — ADR-0035.
