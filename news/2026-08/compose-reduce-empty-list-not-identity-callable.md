# `[∘]` over an empty operand list is the identity `Callable`

`[OP]` applied to zero operands yields `OP`'s identity element. For function
composition the identity is the identity *function*, so `[∘]` alone must be a
working `Callable`:

```raku
my &composed = [∘];
say composed("foo");   # raku: foo
```

mutsu produced `Any` — `reduction_identity` had no arm for `∘`/`o` and fell
through to its `Nil` default — so binding it to `&composed` left the symbol
unusable and calling it died with "Unknown function: composed".

## Fix

`reduction_identity` now returns the identity function for `o` / `∘`. It is
built as a marker carrier `Sub` (an env key resolved by `call_sub_value`),
exactly the way a `f ∘ g` composition is already represented — `reduction_identity`
is a pure function of the operator name, so it can mint the value without a
compiler round-trip or an interpreter handle. `sub_is_call_carrier` was extended
with the new marker so no fast path tries to inline its (empty) body.

Rakudo's identity is a one-positional block, so the carrier reproduces its
arity errors too: calling it with no argument or with two arguments raises the
same "Too few/many positionals passed; expected 1 argument" as `-> $x { $x }`
would.

Non-empty `[∘]` was already correct and is unchanged; `t/numeric-coercion-gaps.t`
pins the one- and two-operand cases alongside the new empty one.
