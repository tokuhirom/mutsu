# A user `multi infix:<+>` candidate beats the core operator for every argument type

Split off from `todo/deep/user-prefix-op-candidate-beats-builtin-typed-candidate.md`
(now `news/2026-09/user-increment-op-candidate-ranking.md`) on 2026-09-07, when the
*increment* half of that finding landed as [ADR-0071](../../docs/adr/0071-native-operators-are-dispatch-candidates.md).
The infix half was deliberately left out of that PR and is recorded here.

## Repro (measured 2026-09-07)

```raku
multi infix:<+>($a, $b) is default { "USER" }
say 1 + 2;
```

- `raku`: `3` — the core `Int:D, Int:D` candidate wins; the user's untyped
  (`Any, Any`) candidate is wider.
- `mutsu`: `USER`.

## Root cause

Exactly the shape ADR-0071 fixed for `prefix:<++>`, one layer down: `try_user_infix`
(`src/vm/vm_arith_ops.rs`) hands *every* matching user candidate the call, because
the native `infix:<+>` implementation is not a dispatch candidate at all — there is
nothing for the user's `($a, $b)` candidate to lose a narrowness comparison against.

ADR-0071's mechanism generalises directly: model the core candidate set as the type
constraints an argument binds to, rank it against the winning user candidate with
`candidate_specificity_rank_for_args` + `candidate_type_distance`, and give ties to
core.

## Why this is bigger than the increment slice, and needs its own design pass

1. **The core candidate sets are large and per-operator.** `&prefix:<++>` has nine
   core candidates and one operand; `&infix:<+>` has dozens (`Int:D, Int:D`,
   `int, int`, `Num:D, Num:D`, `Rat, Rat`, `Complex, Complex`, the mixed
   `Real`/`Numeric` pairs, `Mu:D, Mu:D`, ...) across two operands, and each of
   `-`, `*`, `/`, `%`, `**`, `~`, `==`, `eq`, `<`, `cmp`, ... has its own. A
   nine-entry table written by hand does not scale to that; the design question
   is whether to derive the sets from rakudo's `.candidates` once and vendor the
   table, or to model them as a small set of *numeric-promotion tiers* that the
   ranking can consult generically.
2. **The execution side is not an lvalue store.** The increment slice could run
   the core implementation at the call site because the operand already arrived
   as a container reference. An infix has no lvalue, so the win condition is
   simpler — but `try_user_infix` sits on the arithmetic *hot path*, and the
   guard there cannot be name-gated as cheaply as `exec_call_func_op`'s (which is
   free because it only fires for four routine names).
3. **`is default` and metaoperator derivation interact.** A user `infix:<+>`
   is also the base of `+=`, `[+]`, `>>+<<` and `X+`. Whether each derived form
   re-runs the ranking or inherits the base operator's decision needs measuring
   against rakudo before any of it is implemented.

## Starting points

- `src/vm/vm_arith_ops.rs` — `try_user_infix`, the missing gate
- `src/runtime/native_increment_dispatch.rs` — the ranking mechanism to generalise
- `docs/adr/0071-native-operators-are-dispatch-candidates.md` — the decision, its
  rejected alternatives (an opcode flag; synthetic `FunctionDef`s), and the
  measured acceptance table format to reuse
- `t/user-increment-op-candidate-ranking.t` — the pinning-test shape (each row in
  its own `EVAL`, expectations verified by running the file under `raku` too)
