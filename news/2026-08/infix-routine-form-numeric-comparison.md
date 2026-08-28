# The routine form of a numeric-comparison operator now IS the operator

Three whitelisted roast files (`S32-num/rat.t`, `S32-num/int.t`, `S02-types/WHICH.t`)
regressed under the "real Test" campaign (`MUTSU_REAL_TEST=1`) for one shared
reason: the real vendored `Test.rakumod`'s `cmp-ok` reaches an operator only
through its *routine* form — `&CALLER::LEXICAL::("infix:<$op>")($got,
$expected)` — never through `$a op $b` syntax. mutsu's routine form
(`Interpreter::call_infix_routine`) folded numeric comparisons with the pure
static table `apply_reduction_op` plus its own, separate
`coerce_infix_operand_numeric` bridge — a *reimplementation* of the real
operator's coercion rules that had drifted from it. So `a == b` and
`&infix:<==>(a, b)` could disagree:

```
mutsu -e 'say &infix:<==>(Inf.Rat, Inf)'                 # False   -- raku: True
mutsu -e 'say &infix:<==>((-Inf).FatRat, -Inf)'          # False   -- raku: True
mutsu -e 'my @a=1,2,3; my $s=SetHash.new(1,2,3); say &infix:<==>(@a,$s)'   # False -- raku: True
mutsu -e 'my class Foo is Int {}; say &infix:<==>(Foo.new(42), 42)'        # False -- raku: True
```

In every case the *operator* form already answered correctly — only the
routine form was wrong.

This is the same shape of bug fixed for `eqv` in commit `8360b3120`
("the `.cache` List view is a List everywhere, and `&infix:<eqv>` is the eqv
operator"): the fix is not to patch the bridge case by case (that is exactly
the kind of drift this campaign keeps removing), but to route the routine
form through the interpreter's real operator body, so the two forms cannot
diverge again.

## The fix

`==`, `!=`, `<`, `>`, `<=`, `>=` and `<=>` all now share one implementation
each — `Interpreter::num_eq_values` / `num_ne_values` / `num_lt_values` /
`num_le_values` / `num_gt_values` / `num_ge_values` / `spaceship_values` in
`src/vm/vm_comparison_ops.rs` and `src/vm/vm_comparison_order_ops.rs` — used
by both:

- the stack-based `exec_num_*_op` / `exec_spaceship_op` opcodes (the `$a ==
  $b` operator form), and
- `call_infix_routine` (`&infix:<==>($a, $b)`) and
  `eval_reduction_operator_values` (`[==]`, `Z==`, `>>==<<` and the other
  reduction/metaop forms), in `src/runtime/builtins_operators_infix.rs` and
  `src/vm/vm_dispatch_helpers.rs` respectively.

`!=` is now literally `num_eq_values` negated (it previously duplicated
almost the same body, minus an exact-BigInt-equality branch `==` had — a
second, smaller divergence between the two operators that this unification
also closed). `<`, `<=`, `>`, `>=` keep their Int/Int and Num/Num fast paths,
now inside the shared function instead of duplicated per opcode.

A user-defined `multi sub infix:<==>` on an object operand still wins first
in both `call_infix_routine` and `eval_reduction_operator_values` — the
redirect to the built-in numeric-comparison body only fires when no
user-declared candidate matches (mirrored from the existing
`reduction_op_is_numeric` bridge check).

## A second bug the unification surfaced: `"1" == "1 "` was already broken

Routing `[==]`/`&[==]`/`.unique(with => &[==])` through the real operator
body initially *regressed* `t/reduce-numeric-string-whitespace.t` (tests 9
and 10): `("1", 1, "1 ", 2).unique(with => &[==])` stopped deduping `"1"` and
`"1 "`. The cause was not the redirect — it uncovered a pre-existing bug in
the operator itself: `"1" == "1 "` (and `"1.0" == "1"`) already answered
`False` via the plain `$a == $b` operator form, even before this PR. The old
routine-form fold happened to hide it because its `to_num` helper always
numified (and trimmed) both operands; the real operator's `exec_num_eq_op`
took a "same variant → compare raw values" shortcut that, for two `Str`
operands, compared their literal bytes instead of their numeric value.

Fixed by widening that shortcut's `needs_float` condition: when both
operands are `Str` and BOTH parse as a number (via the existing
whitespace-trimming `to_float_value`), compare numerically. Two `Str`s where
either side is not numeric (mutsu's bare-string enum modeling, e.g. `$status
== Broken`) still fall through to the raw-string-equality shortcut unchanged
— `==` stays deliberately lenient there, per `infix_is_strictly_numeric`'s
existing doc comment.

## A regression caught before it shipped

The reduction-form redirect (`eval_reduction_operator_values`) initially
bypassed a user-defined `multi sub infix:<==>` entirely — `[==] ($a, $b)` on
a class with a custom `==` stopped calling it. Caught by testing that
scenario directly (not in the original repro list) and fixed by preserving
the existing `try_user_infix` check ahead of the new redirect, gated on
either operand needing the Instance/ContainerRef bridge — the same condition
the pre-existing numeric bridge used.

## Verification

- `roast/S32-num/rat.t` test 749 (`±Inf/NaN ⇿ Rat`) now passes under
  `MUTSU_REAL_TEST=1` (previously failing on 4 inner subtests).
- `roast/S32-num/int.t` and `roast/S02-types/WHICH.t` now pass completely
  (exit 0) under both `MUTSU_REAL_TEST=1` and the native provider.
- `rat.t` still aborts later in the file, in an unrelated subtest ("`eqv`
  with zero-denominator Rationals") — a genuinely separate, pre-existing `is
  copy`-parameter-binding bug, reproduces identically before this PR. Filed
  as `todo/tickets/is-copy-param-not-decoupled-through-sigilless-capture-chain.md`.
- New regression pin: `t/infix-routine-form-numeric-comparison.t` (27
  assertions, green under real `raku` too).
- `make test` green; a native-provider roast sweep across `S03-operators`,
  `S32-num`, `S02-types`, `S06-*`, `S29-context` and related files is clean;
  `./scripts/battery-testsuite.sh` unchanged.
