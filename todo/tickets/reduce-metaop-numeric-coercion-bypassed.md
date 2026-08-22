# `[+]`/`[-]`/etc. reduce meta-operator silently gives `0` for Match/user-Numeric objects

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Language/grammars.rakudoc:112` and `:132`).

## Root cause

The compiled `[+]` reduction meta-operator (`Expr::Reduction` → `OpCode::Reduction` →
`exec_reduction_op` in `src/vm/vm_misc_reduction_exec.rs` → `eval_reduction_operator_values`
in `src/vm/vm_dispatch_helpers.rs` → `Interpreter::apply_reduction_op` in
`src/runtime/ops_reduction.rs`) implements `"+"` by calling
`crate::builtins::arith_add(left.clone(), right.clone())` (`src/builtins/arith/add_sub.rs`)
**directly on the raw operand `Value`s**, with no numeric-coercion bridge in front of it.
`arith_add` is a pure/static function with no interpreter access, so it cannot dispatch a
user-defined `method Numeric { ... }` (or a `Match` object's implicit numeric coercion) —
it silently falls through its internal `to_num`/type-match arms to a `0.0` default for any
`Instance`/`Match` operand it doesn't specifically recognize.

By contrast:
- The plain binary `+` operator (compiled `OpCode::Add` or similar) goes through
  `coerce_numeric_bridge_pair`/`coerce_infix_operand_numeric` (in
  `vm_dispatch_helpers.rs`) **before** calling `arith_add`, so `$a + $b` on two `Match` or
  user-Numeric objects works correctly.
- `.reduce(&infix:<+>)` and `.reduce({$^a + $^b})` (the *method*, in
  `src/runtime/builtins_reduce.rs`, `reduce_items`/`reduce_call_step`) dispatch through
  `vm_call_on_value`/`call_sub_value`, which reaches the same coercing binary-`+` path.

Only the compiled `[+]`/`[-]`/`[*]`/... reduction **meta-operator** skips the bridge,
because `apply_reduction_op` calls the raw arithmetic builtins instead of routing through
`eval_truthy`/`coerce_numeric_bridge_pair`-style dispatch (or falling back to
`try_user_infix`/a callable dispatch) for non-primitive operands.

## Minimal repro

```raku
class Foo { has $.n; method Numeric { $!n } }
my @c = Foo.new(n=>2), Foo.new(n=>3);
say @c.reduce(&infix:<+>);   # 5 -- correct
say @c.reduce({$^a + $^b});  # 5 -- correct
say [+] @c;                  # 0 -- WRONG, raku: 5
```

Grammar-flavored repro (this is how it surfaces in `grammars.rakudoc`, since a Match
capture array reduced with `[+]`/`[-]` hits the same path):

```raku
grammar Calculator {
    token TOP { [ <add> | <sub> ] }
    rule  add { <num> '+' <num> }
    rule  sub { <num> '-' <num> }
    token num { \d+ }
}
class Calculations {
    method TOP ($/) { make $<add> ?? $<add>.made !! $<sub>.made; }
    method add ($/) { make [+] $<num>; }
    method sub ($/) { make [-] $<num>; }
}
say Calculator.parse('2 + 3', actions => Calculations).made;
# raku: 5
# mutsu: 0
```

Plain integers/Str/Rat are unaffected (`[+] (2,3)` correctly gives `5`) — only operands
that need a `.Numeric`/`.Bridge` method dispatch (any `Instance`, including `Match`) hit
the `0` default.

## Affected files

- `src/runtime/ops_reduction.rs` — `apply_reduction_op`'s `to_num`/`"+"`/`"-"`/`"*"`/`"/"`
  arms call the raw `arith_*` builtins without coercion.
- `src/vm/vm_dispatch_helpers.rs` — `eval_reduction_operator_values` (calls
  `apply_reduction_op`) and `coerce_numeric_bridge_pair`/`coerce_infix_operand_numeric`,
  which is the existing bridge the fix should route through.
- `src/vm/vm_misc_reduction_exec.rs` — `exec_reduction_op`, the reduction fold driver that
  calls `eval_reduction_operator_values` per step.

## Suggested next step

Before calling `apply_reduction_op`'s arithmetic arms, coerce `Instance`/`Match` operands
through the same `coerce_numeric_bridge_pair` (or `try_user_infix`) path the binary
operator and `.reduce()` method already use — likely by giving `eval_reduction_operator_values`
(which already has `&mut self`) first refusal on Instance operands before falling into the
static `apply_reduction_op` table, mirroring how it already special-cases Junction operands
just above the `apply_reduction_op` call.
