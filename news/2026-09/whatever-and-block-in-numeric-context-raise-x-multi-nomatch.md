# A bare `Whatever` or `Sub`/`Block` in numeric context now raises `X::Multi::NoMatch`

[#9791](https://github.com/tokuhirom/mutsu/issues/9791) (found by the 2026-09-27 doc-diff
sweep) reported that forcing a `Whatever` held in a variable, or a bare `Sub`/`Block`, into
a numeric context threw an untyped `X::AdHoc` or silently answered a number instead of
Rakudo's `X::Multi::NoMatch`:

```raku
my $x = *; try { $x + 2 }; say $!.^name;   # raku: X::Multi::NoMatch, mutsu: X::AdHoc
my $b = { 1 };
say (try $b + 1) // $!.^name;              # raku: X::Multi::NoMatch, mutsu: 1
say (try [+] $b) // $!.^name;              # raku: X::Multi::NoMatch, mutsu: 0
say (try +$b) // $!.^name;                 # raku: X::Multi::NoMatch, mutsu: X::AdHoc (untyped)
```

Neither type declares a `.Numeric` candidate (a `Whatever` reaching an operator directly is
not a curry point — those are wrapped into a `WhateverCode` at parse time), so every one of
Rakudo's generic numeric infix candidates, which end in `.Numeric`, fails to resolve.

The fix adds `crate::runtime::require_numeric_candidate`
(`src/runtime/utils/type_misc.rs`), the single check for this "no `.Numeric` candidate at
all" case, and wires it into every place that numifies an operand for a *genuinely*
arithmetic operator: `coerce_numeric_bridge_pair_strict` (the bridge `+`, `-`, `*`, `/`, `%`
and `**`'s VM opcodes all funnel through, fixing all of them, not just `+`), the unary `+`
and unary `-` VM opcodes' own coercion, and `arith_add` itself (needed separately because a
multi-element reduction fold like `[+] $b, $c` or a `Z+` metaop calls it directly, bypassing
the opcode-level bridge). It deliberately does NOT touch the *plain*
`coerce_numeric_bridge_pair`/`coerce_infix_operand_numeric` that `==`, `<=>` and the other
comparison opcodes also share: a chained comparison's desugaring (`chain_compare.rs`) can
reuse a SmartMatch-autoprimed compound-Whatever operand as a later link's shared value
(`t/lang/operators/chained-compare-ast-node.t`'s `"foo" ~~ *.chars == 3`, which must keep
evaluating to a plain `False` via the existing type-mismatch-is-unequal fallback, not throw
on the still-uninvoked `WhateverCode`) — fixing comparisons for an isolated `Block == 2`
is a real, separate gap this ticket leaves for later. A pre-existing, unrelated message typo
in the Instance-coercion path (`\v:: *%_` instead of Rakudo's `\v: *%_`) was fixed at the
same time, since it shares the same message-construction code.

The regression test `t/types/whatever-block-numeric-nomatch.t` pins the reported
repro plus the multi-element reduction case, and confirms the unrelated `[max]` no-identity
single-element passthrough and ordinary `[+]` numeric reduction are unaffected — verified
against both mutsu and a real Rakudo (2022.12) oracle.
