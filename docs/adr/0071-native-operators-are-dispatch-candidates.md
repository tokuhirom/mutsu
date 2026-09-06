# ADR-0071: A natively implemented operator is a dispatch candidate, not a fallback

- Status: Accepted (increment operators and infix operators both implemented)
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

- **`infix:<...>` had the same missing gate**, and it is now closed the same
  way — see "Infix operators" below.
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

## Infix operators (2026-09-07)

The infix half was deferred out of the original slice because of two open
questions. Both were answered by measurement.

### Question 1: how to model core candidate sets that are large and per-operator

`&infix:<+>` has 31 core candidates and every operator has its own set, so the
hand-written nine-entry table the increment slice used does not scale. The two
options were (a) vendoring a table derived once from rakudo's `.candidates`, and
(b) modelling a small set of *numeric-promotion tiers* the ranking consults
generically.

**Chosen: (a), a vendored table, factored into shared groups.** The tier model
was tried first and rejected on evidence: a tier model has to answer "what is the
narrowest core constraint for this operand", and for a `1 + 2e0` call it would
answer `(Int, Num)` at distance 0 — but rakudo has **no** `(Int:D, Num:D)`
candidate, so the call really binds `(Real, Real)`, and a user
`multi infix:<+>(Int $a, Num $b)` therefore *wins* (measured: `USER`). No tier
model that reasons per-operand can see that, because the fact being used is the
*absence* of a pair. The narrowness question is about the candidate set as a set,
so the set is what has to be modelled.

The cost is much smaller than the ticket feared, because the sets are shared: the
whole numeric family is `Int:D`/`Num:D`, three `Rational:D` rows, three
`Complex:D` rows and `(Real, Real)`, with per-operator additions (temporal and
`Range` rows for `+`/`-`, no `Complex` for `<`, none of `Real`/`Rational` for
`!=`). `src/runtime/native_infix_dispatch.rs` is ~60 table rows in nine shared
groups covering 22 operators, each row a pair of type-constraint strings. An
operator with no entry keeps the old behaviour (the user candidate wins), which
is always right for a purely user-defined `infix:<@@>`.

Alternative B of the increment slice (register synthetic `FunctionDef`s) stays
deferred for the same reasons, and would now also have to render 31 signatures
per operator.

### Question 2: the cost on the arithmetic hot path

`try_user_infix` is on the arithmetic hot path, and unlike the increment slice's
`CallFunc` guard it cannot be name-gated. It did not need to be: the funnel
*already* bails on `user_declared_infix_ops.is_empty()` before it does anything
else, so the ranking is unreachable unless a user `infix:<op>` of that exact name
is in scope. Measured with `rust-gdb -batch` breakpoints on both
`try_user_infix`'s post-guard body and `core_infix_candidate_wins`, running
`benchmarks/int-arith.raku` (an Int-addition loop): **both breakpoints hit zero
times**. The same breakpoint fires on the first `+` of a program that declares
`multi infix:<+>`, so the probe is valid. There is no A/B to run — the
instruction stream for arithmetic with no user operator in scope is unchanged.

### Decision

The same one as for the increment operators, minus the lvalue store (an infix
has no lvalue, so "the core candidate wins" simply means `try_user_infix` reports
`None` and the existing native path runs):

1. The core candidate set of each natively-implemented infix is modelled as the
   pairs of type constraints its two-operand candidates bind, transcribed from
   rakudo's `.candidates`.
2. The narrowest core candidate for the call is the one with the most
   meaningfully-typed positionals, then the least MRO distance.
3. That candidate is ranked against the user's with
   `candidate_specificity_rank_for_args` + `candidate_type_distance`, nominal
   narrowness first. **Ties go to core.**
4. A plain `sub infix:<op>` is a lexical shadow, not a candidate, and still
   replaces the operator outright.

### What the ranking needed underneath

Four gaps in the dispatch metrics surfaced as soon as an operator started
ranking against a core candidate, each a pre-existing wrong answer for ordinary
`multi` dispatch too:

- **`Rational` scored `UNRELATED`.** `(1/2) ~~ Rational` is True but the role was
  not on `Rat`'s modelled MRO, so rakudo's core `(Rational:D, Rational:D)` row
  could not be expressed. Added; a user `multi infix:<+>(Rat $a, Rat $b)` now
  out-narrows it, as in rakudo.
- **An enum value ranked as its base type.** `enum A <e1 e2>` values report
  `Int`, so `multi f(A $x)` lost to `multi f(Int $x)` for `f(e1)` (rakudo picks
  the enum-typed one). An enum value now narrows in three steps — its own value
  name, its enum type, then the base type.
- **An enum *value* used as a parameter (`multi infix:<->(e1, e2)`, roast
  `S03-operators/custom.t`) scored `UNRELATED`.** It is the narrowest constraint
  there is, so it is distance 0. Together with counting a literal positional
  toward nominal narrowness, this is what keeps that whitelisted roast test
  passing once the operator has a core candidate to lose to.
- **`UInt` did not rank as a `subset`.** It is `subset UInt of Int where * >= 0`
  in rakudo, but mutsu implements it as a type-matching special case rather than
  a registry entry, so `multi f(UInt $x)` lost every tie to `multi f(Int $x)`
  for `f(10)` — and `t/inline-module-check-import.t`'s
  `multi infix:<+>(UInt $a, UInt $b)` lost to the core `(Int:D, Int:D)`.
  `Interpreter::constraint_is_subset` now covers the core subsets as well as the
  registry, and `f(10)` picks the `UInt` candidate as rakudo does.

### Measured acceptance criteria

Pinned by `t/user-infix-op-candidate-ranking.t`, which passes **identically under
`raku` and under `mutsu`** (49/49 both ways, two of them `todo`-marked; see the
divergence below).

| call, with `multi infix:<+>($a, $b) is default { "USER" }` in scope | rakudo | before | after |
|---|---|---|---|
| `1 + 2` | core `(Int:D, Int:D)` → `3` | user → `USER` | core → `3` |
| `1.5 + 2.5` | core `(Rational:D, ...)` → `4` | `USER` | `4` |
| `1e0 + 2e0` | core `(Num:D, Num:D)` → `3` | `USER` | `3` |
| `1 + 2e0` | core `(Real, Real)` → `3` | `USER` | `3` |
| `True + 1` | core `(Int:D, Int:D)` → `2` | `USER` | `2` |
| `<42> + 1` | core `(Int:D, Int:D)` → `43` | `USER` | `43` |
| `(1+2i) + 1` | core `(Complex:D, Real)` → `2+2i` | `USER` | `2+2i` |
| `(1..2) + 1` | core `(Range:D, Real:D)` → `2..3` | `USER` | `2..3` |
| `"a" + "b"` | user (only the `Mu` catch-all matches) | user (agreed) | unchanged |
| `1 + "2"`, `Any + Any`, `1 + Nil` | user | user (agreed) | unchanged |
| `P.new + P.new` | user | user (agreed) | unchanged |
| `$x += 2`, `[+] 1, 2, 3`, `@a >>+<< @b` | core (derived forms inherit) | `USER` for `+=` | core |

Ranking rows, `1 + 2` unless noted:

| user candidate | rakudo | before | after |
|---|---|---|---|
| `(Any $a, Any $b)` / `(Mu $a, Mu $b)` | core | user | core |
| `(Cool $a, Cool $b)` / `(Real ...)` / `(Numeric ...)` | core | user | core |
| `($a where * > 0, $b)` | core | user | core |
| `(Str $a, Str $b)`, `"a" + "b"` | user | user (agreed) | unchanged |
| `(Str $a, $b)`, `"a" + 1` | user | user (agreed) | unchanged |
| `(Rat $a, Rat $b)`, `1.5 + 2.5` | user | user (agreed) | unchanged |
| `(Int $a, Num $b)`, `1 + 2e0` | user | user (agreed) | unchanged |
| `subset Sm of Int`, `(Sm $a, Sm $b)` | user | user (agreed) | unchanged |
| `enum E <A B>`, `(E $a, E $b)`, `A + B` | user | user (agreed) | unchanged |
| `sub` (not `multi`) | user, for every type | user | unchanged |

The other operators rank the same way, verified per operator against rakudo:
`-`, `*`, `/`, `**`, `%`, `~`, `==`, `eq`, `<`, `cmp` all run their core
candidate for an untyped `is default` user candidate, and all reach a matching
user candidate for two instances of a user class.

### Known remaining divergence: an exact nominal tie should be ambiguous

rakudo refuses `multi infix:<+>(Int $a, Int $b) { }; 1 + 2` with
`Ambiguous call to 'infix:<+>(Int, Int)'`, listing the core `(Int:D $a, Int:D $b)`
next to the user's; the same holds for `multi infix:<~>(Str $a, Str $b)`. mutsu
gives the tie to the core candidate (rule 3) and runs the native operator
silently. Raising the error needs the core candidate to carry a renderable
signature, which is alternative B; the two rows are `todo`-marked in
`t/user-infix-op-candidate-ranking.t` so the divergence cannot be forgotten.

### Consequences of the infix half

- `try_user_infix` is the single funnel for every user infix — the arithmetic
  opcodes, `InfixFunc`, the hyper/reduce/cross metaop bridge and the flip-flop
  path all go through it — so one gate covers all of them, and the derived forms
  (`+=`, `[+]`, `>>+<<`, `X+`) inherit the base operator's decision, which is
  what rakudo does.
- `exec_mod_op` was the one arithmetic opcode that never called
  `try_user_infix`, so `multi infix:<%>(P $a, P $b)` was unreachable for
  `P.new % P.new`. It calls it now, like `+`/`-`/`*`.
- `candidate_specificity_rank_for_args` is unchanged; the ranking site adds
  literal positionals to the nominal-narrowness key itself, because a literal
  parameter carries the argument's own type on top of its equality constraint.

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
