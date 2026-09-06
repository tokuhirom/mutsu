# ADR-0071: A natively implemented operator is a dispatch candidate, not a fallback

- Status: Proposed (increment operators implemented; infix operators not started)
- Date: 2026-09-07
- Supersedes: nothing
- Related: [ADR-0019](0019-compiled-declarations-and-unified-method-dispatch.md) (one dispatch entry),
  [ADR-0044](0044-listops-are-routines-not-a-syntactic-rewrite.md) (a core routine gets a callable candidate)

## Context

In Raku every operator is a `multi`. `&prefix:<++>` is not a piece of syntax with
an escape hatch for user overrides — it is a routine with a candidate set, and
rakudo ships nine core candidates for it:

```
:(Mu:D $a is rw)   :(Mu:U $a is rw)
:(Int:D $a is rw)  :(int $a is rw)   :(uint $a is rw)
:(Bool $a is rw)
:(Num:D $a is rw)  :(Num:U $a is rw) :(num $a is rw)
```

`prefix:<-->`, `postfix:<++>` and `postfix:<-->` have the same shape (postfix
splits `Bool` into `Bool:U`/`Bool:D`). Declaring `multi prefix:<++>($a) { ... }`
therefore adds a tenth candidate; it does not replace the operator. Because an
unconstrained parameter is `Any`, the user candidate out-narrows `Mu:D`/`Mu:U`
and *loses* to `Int:D`, `Bool` and `Num:D`/`Num:U`. Measured against rakudo:

```raku
multi prefix:<++>($a) is default { $a - 1 }
my $i = 1;    say ++$i;   # 2      — the core Int:D candidate
my $r = 1/2;  say ++$r;   # -0.5   — the user candidate
```

mutsu got the first line wrong (`0`) for a two-layer reason.

**Layer 1 — the dispatch decision was made at parse time, with no argument
types.** With a `prefix:<++>` sub in scope, `prefix_expr`
(`src/parser/expr/postfix/loop_.rs`, via `match_user_declared_prefix_op`)
rewrote `++$foo` into `Expr::Call { name: "prefix:<++>" }`. The `PreIncrement`
opcode was never emitted, so the native increment was out of the picture
entirely. Postfix had the mirror-image bug: `parse_postfix_update_op` ran
*before* the user-op matcher, so `postfix:<++>` never consulted a user candidate
at all (`multi postfix:<++>($a) { "USER" }; my $s = "abc"; say $s++` printed
`abc`).

**Layer 2 — the native operator was not a candidate.** `resolve_function_with_types`
only ranks registered `FunctionDef`s. The native `++` lived as a hard-coded arm
in `call_function_fallback` (`src/runtime/builtins_operators_fallback.rs`),
reached only *after* a failed user resolution — so there was nothing for the
user's `($a)` candidate to lose a narrowness comparison against. That arm also
returned a fresh value without mutating, so a *non-matching* user candidate left
the variable untouched: `multi prefix:<++>(Str $a) {...}; my $i = 1; say ++$i; say $i`
printed `1` / `1` where rakudo prints `2` / `2`.

The same shape exists for infix operators: `try_user_infix`
(`src/vm/vm_arith_ops.rs`) hands *every* matching user candidate the call, so
`multi infix:<+>($a, $b) is default { "USER" }; say 1 + 2` prints `USER` where
rakudo prints `3`.

## Decision

**A natively implemented operator participates in its own multi dispatch as an
explicitly modelled candidate set, and the decision is made at the call site
that owns the lvalue — at run time, with the argument in hand.**

Concretely, for the four increment operators:

1. **The parse-time rewrite stays, and is made symmetric.** When a user declares
   a `multi` for the operator, the operator compiles to an ordinary
   `Expr::Call` — for postfix as well as prefix. This is the right carrier
   because the call-argument compiler already wraps the operand in a `WrapVarRef`
   (or, for an element, in the `__mutsu_index_rw_arg_N` writeback temp), which is
   exactly the container an `is rw` candidate needs. The dedicated increment
   opcodes remain the only path when no user candidate exists, so nothing on the
   hot path changes.

2. **The core candidate set is modelled as the type constraint an argument binds
   to**, in `src/runtime/native_increment_dispatch.rs`: `Int` for a definite
   argument, `Bool` and `Num` at either definiteness, and `Mu` for everything
   else. The native `int`/`uint`/`num` candidates need no separate entry — they
   are exactly as narrow as their boxed twins for this comparison.

3. **Ranking reuses the metrics multi dispatch already uses.** `Mu` is the root,
   so any user parameter type other than `Mu` itself is strictly narrower and
   takes the call. Against a *typed* core candidate the comparison is nominal
   type narrowness first (`candidate_specificity_rank_for_args`' typed-positional
   count), then refinement (subset / `where` / literal), then
   `candidate_type_distance`. **Ties go to the core candidate**, which is what
   rakudo does: a user `multi prefix:<++>(Int:D $a)` loses to the core `Int:D`
   with or without `is default`.

4. **When the core candidate wins, the increment runs at the call site**, through
   the operand's container reference, and stores through the same paths the
   increment opcodes store through (`store_core_increment_result`). When a user
   candidate wins, the call proceeds normally — and a non-`is rw` user candidate
   therefore leaves the variable alone, which is rakudo's behaviour.

5. **A plain `sub prefix:<++>` is a lexical shadow, not a candidate.** It
   replaces the operator outright for every argument type, as in rakudo. The
   gate is `has_multi_function_cached`.

## Alternatives considered

### A. Keep the native path, gate the parse-time rewrite on a runtime type test at the lvalue site

Do not rewrite `++`/`--` to a call at all. Emit the ordinary increment opcode
with a new "a user candidate is in scope" flag, and have
`exec_pre_increment_op_inner` (and its post/decrement/index siblings) test the
runtime type, call the user candidate when it wins, and skip the store-back.

**Cost.** The flag has to reach six opcodes (`PreIncrement`, `PreDecrement`,
`PostIncrement`, `PostDecrement` and the two `…Index` twins). `OpCode` is pinned
at `size_of::<OpCode>() <= 48` bytes by the `opcode_size_guard` test; those
variants are `(u32, Option<u32>)` today, so one more `bool` fits — but every
exhaustive `OpCode` match in `src/opcode.rs` (six of them list these variants)
and the JIT's opcode classification would need updating. Worse, each of those
opcode bodies would have to grow a "user candidate won" early exit *and* build a
container reference for whatever lvalue shape it is looking at — a plain slot, a
`ContainerRef` cell, a package-scope lexical, a per-call anonymous state, an
attribute cell, an array/hash element. The call-argument compiler already builds
exactly those container references, and does so correctly for all ten lvalue
shapes measured below; re-deriving them inside six opcode bodies would duplicate
that machinery.

**Rejected** because it puts the work in six places instead of one, requires an
opcode-shape change, and reimplements container capture that already exists and
is already correct.

### B. Register synthetic `FunctionDef`s for the core candidates

Install real `FunctionDef`s for the nine core signatures into the multi registry
so `choose_best_matching_candidate` ranks them with no special case at all. This
is the most faithful model.

**Cost.** A `FunctionDef` needs a body. Marking one with a sentinel body that the
dispatcher recognises reintroduces a special case at the *execution* end instead
of the ranking end, and the synthetic candidates would then leak into
`&prefix:<++>.candidates`, `nextsame`/`callsame` chains, `.signature`
introspection and the `X::Multi::Ambiguous` message text — each of which needs
its own answer before any of it is correct. That is a substantially larger
surface than the ranking bug being fixed.

**Deferred, not rejected.** It is the right end state if mutsu ever needs
`&prefix:<++>.candidates` to enumerate the core set. The candidate *types* are
modelled in one table either way, so switching later is a local change.

### C. Chosen: model the core candidate set, rank at the call site

The cost is one new module plus a guard on the `CallFunc` opcode, and the guard
is gated on the routine name so it is free for every other call. It reuses
`candidate_specificity_rank_for_args` and `candidate_type_distance` unchanged, so
the operator ranks by the same rules as any other multi.

## Measured acceptance criteria

Pinned by `t/user-increment-op-candidate-ranking.t`, which passes **identically
under `raku` and under `mutsu`** (47/47 both ways). Each row runs in its own
`EVAL` so candidate sets do not leak between rows.

With `multi prefix:<++>($a) is default { $a - 1 }` in scope:

| argument | rakudo picks | before | after |
|---|---|---|---|
| `my $i = 1` (Int) | core `Int:D` → `2`, `$i` = 2 | user → `0`, `$i` = 1 | core → `2`, `$i` = 2 |
| `my $b = True` (Bool) | core `Bool` → `True` | user → `0` | core → `True` |
| `my $b = False` (Bool) | core `Bool` → `True` | user → `-1` | core → `True` |
| `my $n = 1e0` (Num) | core `Num:D` → `2` | user → `0` | core → `2` |
| `my $x = <42>` (IntStr) | core `Int:D` → `43` | user → `41` | core → `43` |
| `my Num $n` (Num:U) | core `Num:U` → `1` | error | core → `1` |
| `my Bool $b` (Bool:U) | core `Bool` → `True` | error | core → `True` |
| `my $r = 1/2` (Rat) | user → `-0.5`, `$r` = 0.5 | user (agreed) | user (unchanged) |
| `my $u` (Any:U) | user → `-1` | user (agreed) | user (unchanged) |
| `my Str $s` (Str:U) | user → `-1` | user (agreed) | user (unchanged) |
| `my Int $i` (Int:U) | user (no core `Int:U`) | user (agreed) | user (unchanged) |
| `my @a; ++@a[0]` | user → `-1`, element untouched | user (agreed) | user (unchanged) |
| `my %h; ++%h<k>` | user → `-1`, element untouched | user (agreed) | user (unchanged) |

`prefix:<-->` behaves identically. `postfix:<++>`/`postfix:<-->` were not
consulting the user candidate at all before, and now agree on every row above
(with the core `Bool:U --> False` / `Num:U --> 0e0` return values).

Ranking rows, all with an `Int` argument unless noted:

| user candidate | rakudo | before | after |
|---|---|---|---|
| `(Str $a)` (no match) | core → `2`, `$i` = 2 | core → `1`, `$i` = **1** | core → `2`, `$i` = 2 |
| `(Int $a)` | core → `2` | user | core |
| `(Int:D $a)`, with or without `is default` | core → `2` | user | core |
| `(Cool $a)` | core → `2` | user | core |
| `(Any $a)` | core → `2` | user | core |
| `(Mu $a)` | core → `2` | user | core |
| `(Mu $a)` with a `Rat` argument | core `Mu:D` → `1.5` | user | core |
| `(Any $a)` with a `Rat` argument | user | user | user |
| `($a where * > 0)` | core → `2` | user | core |
| `subset H of Rat; (H $a)`, `Rat` argument | user | user | user |
| `($a is rw)` | core → `2` | user | core |
| `($a is rw)` with a `Str` argument | user, writes back | user, writes back | unchanged |
| `sub` (not `multi`) | user, for every type | user | unchanged |

Lvalue shapes — the store must reach all of them, verified with an `is rw` user
candidate that wins and with a core candidate that wins: plain scalar, array
element, hash element, nested array element, attribute (`++$!x`), `state`, `our`,
native `my int`, typed `my Int`, and a sigilless alias (`my \c = $b`). All ten
agree with rakudo.

## Known remaining divergences (deliberately out of scope)

- **`infix:<...>` has the same missing gate.** `try_user_infix` in
  `src/vm/vm_arith_ops.rs` hands every matching user candidate the call, so
  `multi infix:<+>($a, $b) is default { "USER" }; say 1 + 2` prints `USER` where
  rakudo prints `3`. The candidate-set modelling in this ADR generalises to it,
  but the core candidate *sets* are far larger (`&infix:<+>` has dozens) and the
  execution side is not an lvalue store, so it is a separate slice.
- **A refinement-constrained user candidate on an increment operator.** rakudo
  refuses to compile `multi prefix:<++>(Int $a where * > 0)`, a `subset`-typed
  candidate, or a literal-parameter candidate at all ("Circularity detected in
  multi sub types for &prefix:<++>"); mutsu accepts them and, per rule 3, runs
  the user candidate. Reproducing a compile-time circularity check is unrelated
  work.
- **`++` on an `Int` *subclass* instance yields `1`.** `class MyInt is Int {};
  ++MyInt.new(5)` is `6` in rakudo and `1` in mutsu — but that is true with no
  user candidate in scope too, so it is a pre-existing `increment_value` gap, not
  a dispatch one.

## Consequences

- The four increment operators now rank by the same rules as any other multi,
  and the `call_function_fallback` `"++" => arith_add(arg, 1)` arm is no longer
  reachable for a call that has an in-scope user `multi` (it remains as the
  fallback for a bare `&prefix:<++>(...)` call with no container).
- `type_hierarchy_distance`, `candidate_type_distance`,
  `normalize_incdec_source*`, `decrement_value` and
  `check_incdec_type_constraint` widened from private/`pub(super)` to
  `pub(crate)` so the one ranking site can reach them.
- The sigilless-alias propagation that `exec_pre_increment_op_inner` and
  `exec_pre_decrement_op_inner` each had inline is now one shared
  `propagate_incdec_sigilless_alias`, and the core-candidate store goes through
  `store_core_increment_result` — one store path for the operator, reached from
  both the opcode and the call site.
- No `OpCode` variant changed, so the `opcode_size_guard` test is untouched.
