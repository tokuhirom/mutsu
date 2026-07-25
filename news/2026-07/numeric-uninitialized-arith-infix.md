# Arithmetic infix on a bare numeric type object throws X::Numeric::Uninitialized

`Int + 1`, `Rat * 2`, `Num ** 3` and friends now throw
`X::Numeric::Uninitialized`, matching Rakudo. Previously the arithmetic infixes
(`+ - * / % **`) silently coerced a bare concrete-numeric type object to `0`, so
`Int + 1` returned `1` instead of erroring. The comparison ops
(`== != < > <= >= <=>`) already threw — see
`news/2026-07/numeric-uninitialized-infix.md` — and this closes the arithmetic
half of the same doc-diff (PLAN.md §8.21).

As before, only the *concrete* numeric type objects hard-error (`Int`, `Num`,
`Rat`, `FatRat`, `Real`, `Bool`). `Any`, `Str`, `Numeric`, `Complex` still fall
through to the generic candidate that warns and coerces to `0`.

## Why this was blocked, and what unblocked it

mutsu desugars `$x OP= $y` to `$x = $x OP $y`, so the assignment metaop emitted
the *same* `Add` opcode as a bare infix. Making that opcode strict therefore also
broke `my Rat $a; $a += 0.1` (roast `S32-num/rat.t`), which must yield `0.1`.

Rakudo does not have this problem because `METAOP_ASSIGN` never applies the base
infix to an undefined container: it substitutes the operator's zero-argument
value first — `infix:<+>()` is `0`, `infix:<*>()` is `1` — and throws
`X::NoZeroArgMeaning` for `/` and `%`, which have no zero-argument candidate.

mutsu now models that substitution explicitly:

- `MetaAssignIdentity` (`src/token_kind.rs`) names the four cases (`Zero`, `One`,
  `NoZeroArgDiv`, `NoZeroArgMod`).
- `autoviv_compound_lhs` (`src/parser/stmt/assign/op.rs`) wraps the LHS of a
  desugared arithmetic `OP=` in a synthetic prefix op carrying that identity.
- The compiler lowers the wrapper to a single `OpCode::MetaAssignIdentity`,
  which replaces a type object on top of the stack with the seed value (or
  throws). A concrete value passes straight through.
- With the metaop's undefined LHS handled up front,
  `coerce_numeric_bridge_pair_strict` (`src/vm/vm_dispatch_helpers.rs`) is free
  to run the same `check_type_object_in_numeric_context` predicate the
  comparisons use.

This replaces — rather than extends — the previous band-aid. `*=`, `**=` and
`%=` used to be desugared into a `defined($x) ?? $x !! 1` ternary, which cost
seven extra opcodes (including a `defined` call) and evaluated the LHS
expression twice; they now cost the same single opcode as `+=`. `+=` and `-=`
were never wrapped at all, which is exactly why the strict check could not be
added before.

## Cost of the seed on the hot path

PLAN.md deferred this fix partly on the grounds that guarding `$i += 1` — the
most common loop-counter op — would be too expensive. The seed opcode is much
cheaper than the ternary it replaces, and two further steps keep it off the
critical path:

- An emit-time peephole (same shape as the existing `SetLocalDecl` fusion) folds
  `GetLocal(slot); MetaAssignIdentity(id)` into a single `GetLocalMetaAssign`,
  so the interpreter pays no extra dispatch for a local target.
- The JIT emits the seed as a Tier B inline tag test
  (`src/vm/vm_jit_tier_b_metaop.rs`): a small-Int / encoded-Num word is
  definitively concrete and falls through with no call. `Zero` / `One` cannot
  throw, so their shim is a void one with no status check; only `/=` and `%=`
  keep a fallible call.

What remains is measurable only where `+=` *is* the workload. A synthetic
20M-iteration `$sum += $i` loop costs +2.5% instructions and about +11% wall
(4.54s → 5.04s, release, JIT on). `benchmarks/bench-fib`, `bench-class`,
`bench-array`, `bench-string` and `int-arith` show no consistent movement in
either direction — the bench CI is the authority for those. Given that the fix
buys correct behaviour for an entire operator family (and that `Int + 1` is
nearly always a latent bug the user wants surfaced), that trade is worth making.

## Fallout fixed along the way

- `/=` and `%=` on an undefined container now throw a typed
  `X::NoZeroArgMeaning` with Rakudo's message (`No zero-argument meaning for:
  infix:</>`) and `.name` attribute. `/=` previously produced a bogus `0/1` Rat,
  and `%=` died with a hand-rolled `X::AdHoc`.
- `$obj.attr OP= $y` (`src/parser/stmt/assign/assign_stmt.rs`) and
  `($x = ...) OP= $y` (`src/parser/stmt/assign/compound_expr.rs`) built their
  binary node by hand and so bypassed the metaop desugaring entirely. Both now
  route through `compound_assigned_value_expr`, which also gives them
  user-`infix:<OP=>` override support for free.
- A `Failure` is *concrete*, so it is no longer replaced by the identity:
  `my $a = Failure.new('boom'); $a *= 5` throws the Failure like Rakudo instead
  of quietly yielding `5`. The old ternary tested `.defined`, which a Failure
  fails.
- `OpCode::AtomicCompoundVar` — the fused cross-thread-atomic RMW for
  `$shared OP= rhs` — carries the identity so the fused and unfused forms agree;
  a literal `$x = $x + 1` still gets `None` and no seeding.

## Tests

`t/numeric-uninitialized-arith.t` pins the bare-infix throws, the identity
seeding for every lvalue shape (plain scalar, typed array element, typed hash
key, attribute), the `/=` / `%=` exception shape, and the cases that must *not*
be seeded (a type-object RHS, a `Failure`, a user `infix:<OP=>` override).
