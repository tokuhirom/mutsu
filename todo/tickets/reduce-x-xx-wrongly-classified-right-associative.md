# The `[x]`/`[xx]` reduce meta-operators fold right-to-left, but `x`/`xx` are left-associative in Rakudo

Found while sweeping the surface of
[`string-repeat-x-does-not-dispatch-user-str`](string-repeat-x-does-not-dispatch-user-str.md)
(now [`news/2026-08/infix-x-dispatches-user-str.md`](../../news/2026-08/infix-x-dispatches-user-str.md)):
a 3-argument `[x]`/`[xx]` reduce folds in the wrong order. This is unrelated
to the `.Str`-dispatch bug fixed there — plain `Str`/`Int` operands (no user
method involved) already reproduce it, so it is a separate, differently-shaped
bug and was deliberately left out of that PR.

## Minimal repro

```raku
say [x] "a", 2, 3;   # raku: aaaaaa (6 chars) -- mutsu: 222 chars
say 2 x 3 x 2;        # raku: 222222     -- both mutsu and raku agree here (plain chained infix)
say ((2 x 3) x 2);    # 222222  (left-assoc reading)
say (2 x (3 x 2));    # 222222222222222222222222222222222 (right-assoc reading -- NOT what raku picks)
```

`2 x 3 x 2` parses/executes as `(2 x 3) x 2` in real Rakudo (confirmed by direct
comparison above) -- i.e. `x`/`xx` are **left**-associative, exactly like `~`.
The reduce meta-operator `[x]` must fold the same way: `[x] "a", 2, 3` is
`("a" x 2) x 3` = `"aa" x 3` = `"aaaaaa"` (6 characters). mutsu instead computes
something equivalent to `"a" x (2 x 3)` = `"a" x "222"` = `"a" x 222` = a
222-character string (verified via `([x] "a", 2, 3).chars` == `222`), which is
exactly the right-to-left (right-associative) fold.

## Root cause (two matching hardcoded tables)

Both reduce-associativity tables in the codebase hardcode `x`/`xx` as
right-associative, disagreeing with Rakudo:

- `src/vm/vm_misc_ops.rs:267`: `"=" | ":=" | "=>" | "x" | "xx" => ReductionAssoc::Right,`
- `src/runtime/builtins_reduce.rs:618`: `"=" | ":=" | "=>" | "x" | "xx" => OpAssoc::Right,`

Both fall through to this hardcoded `match` only when
`self.infix_associativity(&infix_name)` (querying the operator's *declared*
Raku associativity trait) returns `None` for `infix:<x>`/`infix:<xx>` -- i.e.
these two entries are themselves the fallback default for when the "real"
associativity lookup doesn't know about `x`/`xx`. The fix is presumably to
either register `infix:<x>`/`infix:<xx>` with `is assoc<left>` wherever the
other core infixes are declared (if that registry exists and is what
`infix_associativity` consults), or simply to move `x`/`xx` into the (implicit)
left-associative default arm in both of the hardcoded `match` tables above
(dropping them from the `Right` arm entirely, since `_ => ReductionAssoc::Left`
/ `_ => OpAssoc::Left` is the fallback).

`=`/`:=`/`=>` staying `Right` in the same table lines is presumably correct
(assignment and pair construction genuinely right-associate in Rakudo) and
should NOT be touched -- only `x`/`xx` need to move.

## Affected files

- `src/vm/vm_misc_ops.rs` (`ReductionAssoc` table, ~line 267)
- `src/runtime/builtins_reduce.rs` (`OpAssoc` table, ~line 618, plus the actual
  fold loops at ~line 88 (`OpAssoc::Right`) and ~line 262/311 that consume the
  classification -- these fold loops themselves look correct and probably need
  no change once the classification is fixed)

## Why this is left as a separate ticket rather than folded into the sibling PR

The sibling PR's fix (routing infix `x`'s left operand through
`coerce_stringy_operand`) is orthogonal: it makes `[x]`/`&infix:<x>`/`x=`
dispatch a user `Str` correctly, but does not touch fold order at all, and the
associativity bug reproduces identically with zero user classes involved. Both
bugs happen to live in the same reduce code path but have unrelated root
causes and unrelated fixes.
