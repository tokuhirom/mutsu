# Writing through a sigilless bind alias now enforces the target's type constraint

`my SmallInt $a = 5; my \x := $a; x = 1000;` used to print `1000` silently in
mutsu, where real Raku dies with `Type check failed in assignment to $a;
expected SmallInt but got Int (1000)`. Likewise a sigilless routine parameter
aliasing a typed caller variable (`sub f(\x) { x = 1000 }; f($a)`) let the
type-violating write through.

## Root cause

`OpCode::TypeCheck` emission was driven by a compile-time, name-keyed map
(`Compiler::local_types`) populated only for the DECLARED variable's own name.
A sigilless bind (`my \x := $a`) declares a brand-new name `x` with no type
annotation of its own, so no `TypeCheck` opcode was ever emitted for a store
through `x`.

But that compile-time map turned out not to be the actual runtime gap: mutsu
already enforces a typed scalar's constraint at every *direct* store via a
separate, runtime, name-keyed registry (`var_type_constraint`, populated by
`OpCode::SetVarType` and consulted on every `SetLocal`). The real bug was that
mutsu's sigilless-bind aliasing mechanism (the `__mutsu_sigilless_alias::`
forward-chain walk, used both for a same-scope `:=` bind and for a sigilless
routine parameter that aliases a caller variable) mirrors a written value
into the alias TARGET's storage directly, with no call into that constraint
registry at all — so a write reaching a typed variable *through* an alias
skipped the check that a write to the variable's own name would have hit.

## Fix

Added `Interpreter::check_sigilless_alias_target_constraint` (`src/vm/vm_helpers.rs`),
which re-runs the same name-keyed `var_type_constraint` lookup and
`type_matches_value` check against the alias TARGET's name at the point a
value is mirrored into its storage. Wired it into the two write-through call
sites that matter for this repro: the statement-context `SetLocal` forward
alias-chain walk (`src/vm/vm_var_assign_set_local.rs`) and the
expression-context assignment path (`src/vm/vm_var_assign_local.rs`). Both are
already gated behind an existing "has any sigilless alias ever been created"
fast-path check, so a program with no sigilless binds pays zero added cost.

This is a genuine runtime fix, not the compile-time "copy `local_types` for a
literal `Expr::Var` RHS" fallback the original investigation considered and
rejected as too narrow: because the check re-resolves the alias target's
constraint from the runtime registry at the exact write site, it covers both
a same-scope `:=` bind AND a sigilless routine parameter aliasing a caller
variable (verified against real `raku` for both), not just the
compile-time-resolvable case.

## What is still open

Investigating this surfaced three separate, unrelated bugs, filed as new
`todo/deep/` tickets rather than folded into this fix:

- Writing through a sigilless alias that has been captured into a genuine
  closure (e.g. `sub { x = ... }`, as opposed to an inline block or a routine
  call) still bypasses the check — closures write through a different
  (cell-based) mechanism this fix does not reach.
- `for LIST -> \x, $value { x = ... }` does not write through to the source
  list's elements AT ALL (even for untyped variables) — this is the shape
  `Native::Overflow`'s test suite exercises, and its root cause is unrelated
  to type-checking: the write-through itself is missing, so the type-check
  question is unreachable for that repro until the write-through is fixed
  first.
- A two-hop sigilless bind chain (`my \y := $a; my \x := y;`) and a sigilless
  bind to a typed array element (`my \x := @arr[$i]`) both incorrectly reject
  a subsequent write with `X::Assignment::RO` ("Cannot modify an immutable
  value"), independent of typing.

See `t/sigilless-alias-typecheck.t` for the regression coverage this PR adds.
