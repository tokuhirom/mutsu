# ADR-0118: Every form of an integer operator, and `.succ`/`.pred`, is one routine

- **Status**: Accepted (2026-09-24, user decision: audit every non-`Str` family for the
  duplication ADR-0117 removed from strings, and unify what is found).
- **Deciders**: tokuhirom, Claude
- **Context**: [ADR-0117](0117-str-methods-and-nqp-ops-share-one-routine.md) (the same decision
  for `Str`), [ADR-0071](0071-native-operators-are-dispatch-candidates.md) (a user `multi` may
  out-narrow a builtin operator).

## 1. Context

A Raku integer operator has many surface forms -- the infix (`7 div -2`), its routine
(`&infix:<div>`), the reduction and triangle (`[div]`, `[\div]`), the hyper (`»div«`), the
reversed metaop (`Rdiv`) -- and some have method twins (`.abs`, `.succ`) or sub twins (`abs()`).
In rakudo they all bottom out in one candidate of `&infix:<div>`, one `Int.succ`.

mutsu implemented them per layer: the VM opcode (`vm_arith_int_ops.rs`, `vm_bitwise_ops.rs`), the
reduction fold (`ops_reduction.rs`, which the metaop and routine forms share), the methods
(`dispatch_core_math.rs`, `dispatch_core_numeric.rs`), the `abs()` builtin
(`functions/dispatch_1arg.rs`), and for `++`/`--` two more copies (`increment_value` in the VM and
`increment_mut_target_value` for `.=succ`). The copies had drifted, measured against rakudo:

| expression | rakudo | mutsu before |
|---|---|---|
| `$min div -1` (`$min` = i64::MIN) | 9223372036854775808 | **Rust panic** (`div_floor` overflow) |
| `$min mod -1` | 0 | **Rust panic** |
| `abs($min)` | 9223372036854775808 | **Rust panic** |
| `[div] 7, -2` | -4 | -3 (Euclidean) |
| `-$min` | 9223372036854775808 | 9.223372036854776e+18 (a Num) |
| `$min.abs` | 9223372036854775808 | -9223372036854775808 (wrapped) |
| `9223372036854775807.succ` | 9223372036854775808 | -9223372036854775808 (wrapped) |
| `(2**70).succ` | 1180591620717411303425 | 1180591620717411303424 (no-op) |
| `abs(-2**70)` | 1180591620717411303424 | 0 |
| `"²".succ` | `³` | `²` (while `++` gave `³`) |
| `$x = "a"; $x .= pred` | Failure | `"a"` (while `--` gave a Failure) |
| `5.5 +& 3` | 1 | 0 (while `[+&] 5.5, 3` gave 1) |
| `7.5 mod 2` | 1.5 | 1 (while `[mod] 7.5, 2` gave 1.5) |

## 2. Decision

1. **`src/builtins/arith/` is the single home** of the Raku-level integer operators:
   `int_div` (floored, `Int()`-coerced operands, BigInt on overflow, a `Failure` on a zero
   divisor), `arith_mod` / `int_mod_i64` (`mod` is `%` on the numeric values), `int_bitop`
   (`+&` `+|` `+^`), `int_shift_left` / `int_shift_right`, `int_negate` / `int_abs` (i64::MIN
   promotes), and `value_succ` / `value_pred` (a number's successor is literally
   `arith_add($n, 1)`; strings step through `str_increment`, superscript digits included).
2. **Every form calls it**: the VM opcodes, the reduction fold (and through it the routine,
   hyper, triangle and `R` forms), the methods, the `abs()` builtin, `++`/`--` and `.=succ` /
   `.=pred`. The VM keeps only what is genuinely its own: the ADR-0071 user-candidate check, the
   junction threading and the `throw_if_failure` operand check.
3. **Native semantics stay separate by contract.** `nqp::add_i` and friends, and the native
   `int` arithmetic, *wrap*; they live in `runtime::nqp_pure` and are not Raku `Int` operators.
4. **Enforced, not requested**: `scripts/check-str-prims.sh` from ADR-0117 is generalized to
   `scripts/check-prims.sh` (`make check-prims`, a `make test` prerequisite and a CI step). Its
   `int` rule fails on a hand-written `Integer::div_floor` / `mod_floor` outside
   `src/builtins/arith/` (opt-out: `int-prim: allow`), and its `names` rule bans the deleted
   copies (`shift_{left,right}_{i64,bigint}`, `superscript_{succ,pred}`) by name.
   `t/types/numeric/int-operator-forms-parity.t` pins 43 rakudo-measured values across the forms.

## 3. Consequences

- The three panics and the ten divergences in §1 are fixed.
- The audit that led here found the same pattern in other families, each to be unified the same
  way under this decision: `NqpPure` is re-implemented by TRIR and the JIT; the `nqp::` list
  ops, `AT-POS` and `[]` each resolve indices differently; `nqp::istrue` / `isconcrete` /
  `istype` / `eqaddr` / `clone` bypass the VM's own routines; there are four byte decoders; the
  regex engine's `\d` / `\w` / `<alpha>` ignore the `CCLASS` table `nqp::iscclass` uses.
