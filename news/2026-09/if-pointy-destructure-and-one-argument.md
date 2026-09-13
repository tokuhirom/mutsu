# `if` reads `-> (...)` as a destructure, and passes the condition as one argument

Two halves of the same gap, both in `src/parser/stmt/control/conditionals.rs`.
[#8340](https://github.com/tokuhirom/mutsu/issues/8340) scoped them together because fixing either
alone leaves the clause called with a signature that never matched the source.

## The parameter parser read the destructure parens as the signature's own

`parse_if_binding_params` stripped a leading `(` and handed the inside to the ordinary parameter-list
parser. But a pointy block has no parenthesised parameter list: `-> ($a, $b)` is **one** parameter
with a destructuring sub-signature — `-> $a, $b` is the two-parameter spelling. Reading the parens
away turned `-> (:key($k))` into a top-level *named* parameter and `-> ($a, $b)` into two
positionals.

`for`, `given`, `with` and a bare `-> (...)` lambda all already recorded this correctly. The rule
they shared — one `__subsig__` parameter when there is more than one sub-parameter, or exactly one
*named* one — is now `fold_parenthesised_pointy_params`, called from both the lambda parser and the
conditional one instead of living in a comment in one of them.

## The clause was called with the condition slipped

`lower_if_clause_binding` invoked the block with `|$tmp`, slipping the condition into as many
arguments as it had elements. rakudo passes the condition as **one** argument and lets the signature
decide what to do with it:

```
$ raku  -e 'if (1, 2) -> $a, $b { say "x" }'
Too few positionals passed; expected 2 arguments but got 1
$ mutsu -e 'if (1, 2) -> $a, $b { say "x" }'     # before
x
```

The slurpy spellings that `roast/S04-statements/if.t` pins all follow from that single rule rather
than needing their own: `*@a` flattens the single list argument, `**@a` keeps it whole, `+@a` applies
the one-argument rule to it. `**@a` had been special-cased here for exactly that reason, and that
special case is now gone — the general rule covers it. That roast file is whitelisted and was green
before the change, which made it the guard for the risky half; it is still green, all 44 tests.

`elsif`, `unless` and `else -> ...` share the lowering and move with it.

## Pin

The `if` cases join the `given`/`with` ones in
`t/routines/signature/given-with-destructuring-pointy-param.t`, as the ticket asked — 29 tests now,
green under mutsu and under real Rakudo.

## Two of the ticket's four repros stay divergent, for reasons outside it

- **`if (a => 1) -> (:key($k))`** now destructures instead of erroring on arity, but mutsu binds `$k`
  where rakudo refuses: a `Pair`'s capture also has a `value` part, and naming only `key` leaves it
  unaccounted. That is the binder missing the rule the *multi-candidate matcher* already has
  (#8325), and it is filed as [#8357](https://github.com/tokuhirom/mutsu/issues/8357).
- **`if 5 -> ($a)`** is `Cannot unpack or Capture` in rakudo and still binds `5` here. A lone
  positional in parens is deliberately kept as a plain parameter, mirroring the `len == 1` capture
  sub-signature case; changing it is a cross-cutting decision about every `.map(-> ($x) {...})` in
  the tree rather than an `if`-specific one, so it is left as it was and documented on the shared
  helper.
