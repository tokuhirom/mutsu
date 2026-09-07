# Big-integer `+`/`-`/`*` borrow their operands instead of deep-cloning them

`todo/perf/bigint-repeated-addition-performance-gap.md` reported that the growing-magnitude
Fibonacci loop from `raku-doc/doc/Language/faq.rakudoc` ran ~14x slower than raku, and asked for
a profile before any guessing. Callgrind (`perf` has never produced usable stacks in this
container) answered both halves of the question: the headline number was a **debug-build
artifact**, but underneath it there was a real and quantified inefficiency, now fixed.

## The measurement

The ticket's 14x came from `target/debug/mutsu`, which it flagged as a caveat. On a release
build mutsu was already *ahead* of raku on the same program (10,000 iterations: 0.08s vs 0.56s),
so the reported gap does not exist. What the profile did show is that the big-integer add path
spent most of its instructions on everything except the addition.

`valgrind --tool=callgrind` on the 100,000-iteration loop (final value ~20,900 digits), against
`target/profiling/mutsu`:

| | baseline | after | |
| --- | --- | --- | --- |
| whole program | 6,406,323,308 Ir | 5,910,311,010 Ir | **-7.7%** |
| `exec_add_op` (inclusive) | 885,064,514 Ir (13.82%) | 368,407,514 Ir (6.23%) | **-58.4%** |
| — of which the addition itself | 214,801,393 Ir | 327,825,722 Ir | 24% → **89%** of the opcode |
| `as_bigint` (operand cloning) | 289,704,385 Ir (4.52%) | gone | |
| `coerce_numeric_bridge_pair_strict` | 72,234,207 Ir (1.18%) | gone from this path | |

The baseline's shape is the finding in one line: reading the operands cost **more** than adding
them (4.52% against 3.35%).

## What was wrong

Two layers of avoidable work sat around a `BigInt + BigInt`:

1. **`as_bigint` deep-cloned both operands.** `arith_add_coerced` (and its `-`/`*` twins) read
   each operand with `as_bigint`, which does `(**i).clone()` — an allocation plus a full
   limb-vector memcpy per operand — only to hand the copies to an operator that merely reads
   them. For a multi-thousand-digit operand that is two allocations and two wide memcpys per
   addition, on top of the one buffer `num-bigint` allocates for the result anyway.

2. **Every big-integer op walked the whole polymorphic guard chain.** `arith_add` tries
   Whatever, mixin-wrapped Range, Range offset, `Date`, `Instant`, `Duration` and `DateTime`
   before reaching numbers, and the VM's `exec_add_op` — whose `Int + Int` and `Num + Num` fast
   paths do not cover a big integer — first routed the pair through
   `eval_binary_with_junctions`, `try_user_infix`, `is_temporal_operand` and
   `coerce_numeric_bridge_pair_strict`. None of those can match a bare `Int`/`BigInt` pair, but
   the walk was ~180M instructions over 100k additions.

## The change

- `src/builtins/arith/rat.rs` grows `big_int_binop` (with `big_int_add`/`big_int_sub`/
  `big_int_mul` wrappers): it views both operands, borrows a `BigInt`'s magnitude straight out of
  its `Arc` and widens only an `Int` into a small owned temporary, then applies a
  `&BigInt op &BigInt`. It returns `None` for any other pair, so callers fall through to the
  rational/float paths exactly as the `as_bigint` guard it replaces did. `as_bigint` itself
  stays for `/`, whose `make_big_rat_arith` genuinely needs owned values.
- `arith_add`, `arith_sub` and `arith_mul` call it once at the top, above the Range/temporal
  guards, and once in place of the old cloning branch.
- `exec_add_op`, `exec_sub_op` and `exec_mul_op` gain a third fast path, next to the existing
  `Int` and `Num` ones and under the same `!has_override` guard, so an `Int`/`BigInt` pair skips
  the junction/coercion wrapper entirely.

`&BigInt + &BigInt` is now 89% of what the add opcode costs; the remaining ~40M instructions per
100k additions are stack traffic and the operand type check. Wall clock on the ticket's program
scaled up to 400,000 iterations: 3.55s → 3.12s, against raku's 4.48s.

## Semantics

`t/bigint-arith-borrowed-operands.t` pins what the shortcut must not change — both operand
orders for `BigInt`-with-`Int`, renormalisation back to a plain `Int` when the result fits,
fall-through to the exact `Rat` and `Num` paths for mixed operands, junction threading, and a
user-declared `infix:<+>`/`infix:<*>` still overriding big-integer arithmetic. Every assertion
was checked against the rakudo oracle first.

Writing it surfaced an unrelated correctness bug: a decimal literal whose integer part exceeds
`i64` silently loses that integer part (`1000000000000000000000000000000.5` evaluates to `0.5`).
That is filed as `todo/tickets/decimal-literal-with-big-integer-part-loses-it.md`, root-caused to
an `unwrap_or(0)` in `src/parser/primary/number.rs` that hides the very overflow the BigInt
fallback next to it is selected by.
