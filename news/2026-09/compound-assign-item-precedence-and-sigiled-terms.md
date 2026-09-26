# Compound assignment binds tighter than the comma; `constant term:<$x>` works

Three gaps found through FixedInt's test suite (#9566, part of the #7988
parse-gap campaign) are fixed.

**Compound assignment precedence.** Rakudo gives an assignment metaop `op=`
item-assignment precedence whenever its base operator is tighter than the
comma, whatever the lvalue's sigil. mutsu let the right operand swallow the
whole comma list, so `($f += 5, 9)` stored 2 (the list's length) instead of 5,
`$x //= 1, 2` stored the list `(1 2)`, and `$s ~= "b", "c"` appended `"b c"`.
The expression grammar now parses a compound assignment's right operand at the
item level (only `,=` keeps the comma list), and the statement form hands a
trailing comma list to the expression grammar, which sinks the extra items
with rakudo's "Useless use" worry.

**Sigiled constant terms.** `constant term:<$bar> = FixedInt.new(:19bit)`
declares the term `$bar`. mutsu parsed `$bar` as an undeclared scalar, so the
term read as `Nil`. The term is now registered as a term symbol, matched
before the variable parsers see the `$`, and assigning to it (`$bar -= 1`,
`$bar = 3`, `$bar .= meth`) routes through the bound object's user `STORE`
the way a sigilless `my \foo` already did.

**Bitwise operators on objects.** `+&`, `+|`, `+^`, `+<` and `+>` now numify
an object operand through its `.Numeric` before the shared integer primitive
runs, as the prefix `+^` already did: `C.new +> 2` was 0 for a class whose
`Numeric` returns 224.

FixedInt's `t/01-basic.t` now passes all 24 tests (12 before).
