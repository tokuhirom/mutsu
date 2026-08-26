# Infix `x` (string repetition) now dispatches a user `Str`, in all four of its code paths

`$a x 2` rendered `.gist` (`S()S()`) instead of a user class's `Str` method,
while the structurally identical `$a ~ $a` already dispatched it correctly:

```raku
class S { has $.s; method Str { $!s } }
my $a = S.new(s => "ab");
say $a ~ $a;   # abab   -- infix `~` dispatches the user Str (correct)
say $a x 2;    # raku: abab
               # mutsu (before this fix): S()S()
```

This is the same root cause as the `[+]`/`[~]` reduce-operator fix recorded in
[`reduce-metaop-numeric-coercion-bypassed.md`](reduce-metaop-numeric-coercion-bypassed.md):
a pure function of two `Value`s cannot dispatch a user method and falls back to
`.gist`. `~` avoids it because `exec_concat_op` runs both operands through
`Interpreter::coerce_stringy_operand` first; infix `x` had no equivalent step.

## The asymmetry that shapes the fix

Unlike `~`/`eq`/`lt`/etc, `x` is **asymmetric**: only the LEFT operand is a
string to repeat. The RIGHT operand is a repeat count and must stay numeric —
coercing it through `.Str` would be wrong. Every fix below therefore coerces
only the left operand, verified with a side-effecting `method Str` that
increments a counter: raku calls it **exactly once** regardless of the repeat
count (confirmed for counts 2, 0, and -1 alike), never once per copy.

## Four code paths, one bug each

`x` reaches user code through four independent implementations, and all four
had the identical gap:

1. **`exec_string_repeat_op`** (`src/vm/vm_arith_int_ops.rs`) — the compiled
   bytecode opcode behind plain `$a x $n` and, because the assignment
   meta-operator `x=` compiles to the same opcode, `$a x= $n` too.
2. **`call_repeat_infix`** (`src/runtime/builtins_operators_repeat.rs`) — the
   routine-call path behind `&infix:<x>($a, $n)`.
3. **`eval_reduction_operator_values`** (`src/vm/vm_dispatch_helpers.rs`) — the
   reduce meta-operator, `[x] $a, $n`.
4. `xx` (list repetition, `src/vm/vm_arith_int_ops.rs` /
   `src/runtime/builtins_operators_repeat.rs`) and the `X~`/`Z~` cross/zip
   forms were audited too and found **already correct**: `xx` does not
   stringify its LHS at all (confirmed against raku: `($a xx 2)[0]` stays an
   `S` instance, not a `Str`), and `X~`/`Z~` already dispatch the user `Str`
   because they delegate to the plain `~` operator, which was fixed in the
   sibling PR.

Each of the three broken paths now runs its left operand through
`Interpreter::coerce_stringy_operand` (widened from `pub(super)` to
`pub(crate)` so the `runtime`-module call site could reach it) before
repeating, including the `reconcile_caller_after_internal_dispatch` drain —
a user `Str` method can mutate a captured-outer caller lexical, and this is an
internal redispatch with no surrounding `CallMethod` op to drain it
automatically, exactly as `exec_concat_op` already does. The reduce path
(`eval_reduction_operator_values`) gets a dedicated `x`-only branch rather than
joining the existing symmetric `reduction_op_is_stringy` list, since that list
coerces *both* operands and `x`'s right operand must not be.

## What else was checked and found correct

- A `does Stringy` role composition (not just a bare class) dispatches its
  `Str` correctly too.
- `x` with a non-numeric right operand (`"a" x "hello"`), and with `0` or a
  negative count, throw/resolve identically to before the fix (unaffected —
  `coerce_stringy_operand` is a no-op for the non-`Instance` right operand in
  all these cases).
- `Buf x N` is unaffected: `Buf` has no *user-defined* `Str`/`Stringy` method,
  so `coerce_stringy_operand`'s `has_user_method` check is false and the value
  passes through unchanged, same as before this fix.

## A separate, differently-shaped bug found along the way

While sweeping the surface, `[x] "a", 2, 3` (a 3-argument reduce with plain
`Str`/`Int` operands — no user class involved) turned out to fold
right-to-left instead of left-to-right: mutsu computes the equivalent of
`"a" x (2 x 3)` (222 characters) where Rakudo computes `("a" x 2) x 3` =
`"aaaaaa"` (confirmed `2 x 3 x 2` chains left-associatively in real Rakudo, the
same way `~` does). Both of mutsu's reduce-associativity tables hardcode
`x`/`xx` as right-associative, which disagrees with Rakudo. This reproduces
identically with zero user methods involved, so it is unrelated to the
`.Str`-dispatch bug fixed here and was filed separately as
[`todo/tickets/reduce-x-xx-wrongly-classified-right-associative.md`](../../todo/tickets/reduce-x-xx-wrongly-classified-right-associative.md)
rather than folded into this fix.

Pinned by twelve new assertions in `t/numeric-coercion-gaps.t`, next to the
`~`/`[~]`/`[lt]` cases the sibling reduce fix added.
