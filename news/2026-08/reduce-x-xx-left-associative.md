# `[x]`/`[xx]` reduce metaops now fold left-to-right, matching Rakudo's operator associativity

The `[x]` and `[xx]` reduce meta-operators folded right-to-left, disagreeing
with Rakudo. `x` (string/list repeat) and `xx` (list repeat producing a list)
are left-associative in Raku, exactly like `~`: `2 x 3 x 2` parses as
`(2 x 3) x 2`. But `[x] "a", 2, 3` computed `"a" x (2 x 3)` = `"a" x "222"` =
`"a" x 222`, a 222-character string, instead of Rakudo's `("a" x 2) x 3` =
`"aa" x 3` = `"aaaaaa"` (6 characters).

## Root cause

mutsu has no persistent registry of built-in infix operators' associativity —
`operator_assoc` (`src/runtime/mod.rs`) is populated only by user-declared
custom infixes carrying an explicit `is assoc<...>` trait
(`src/runtime/registration_sub.rs`). For every built-in operator,
`infix_associativity()` (`src/runtime/accessors_state.rs`) returns `None`, and
callers fall back to a hardcoded `match` table that *is* the canonical
associativity registry for built-ins. Two independent copies of that table
both wrongly classified `x`/`xx` alongside the genuinely right-associative
`=`/`:=`/`=>`:

- `reduction_op_associativity()` in `src/vm/vm_misc_ops.rs` — used by the
  `[x]`/`[xx]`/triangle-`[\x]` VM reduce/produce opcodes.
- `op_associativity()` in `src/runtime/builtins_reduce.rs` — used by
  `reduce(&infix:<x>, ...)`, `produce`, and `zip:with`'s pairwise-combine
  fallback, via three call sites that all delegate to this single function
  (`callable_produce_assoc`/`callable_reduce_assoc`/direct call), so there is
  no third copy of the table to fix.

## Fix

Moved `x`/`xx` out of the `Right` arm in both tables so they fall through to
the (already correct) `_ => Left` default, leaving `=`/`:=`/`=>` (which
genuinely right-associate: `[=>] 1, 2, 3` is `1 => (2 => 3)`) untouched. No
new registry was introduced — the two hardcoded tables were already the
source of truth for built-in operator associativity, so this was a direct,
minimal correction of their content rather than a bigger refactor.

Verified against `raku` directly for `[x]`, `[xx]`, triangle `[\x]`,
`&infix:<x>` via `reduce()`, `x=`, and the empty/1-element/2-element/3+-element
cases, plus confirmed `[~]`, `[**]`, and `[=>]` kept their existing (correct)
associativity. Regression test: `t/reduce-x-xx-left-associative.t`.
