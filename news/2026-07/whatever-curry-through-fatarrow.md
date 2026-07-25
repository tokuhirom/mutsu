# `=>` now participates in Whatever-currying

Raku's `=>` (fat-arrow) pair constructor participates in Whatever-currying: for a
**non-bareword** key, a `*` on either side makes the whole pair a `WhateverCode`
that yields the `Pair` when called. mutsu used to build a literal
`Pair(Whatever, Whatever)` instead, so these all came out as the wrong type:

```raku
(* => *).WHAT          # was (Pair)   now (WhateverCode)
("key" => *).WHAT      # was (Pair)   now (WhateverCode)
(5 => *).WHAT          # was (Pair)   now (WhateverCode)
("x" ~ * => *).WHAT    # was (Pair)   now (WhateverCode)
("k" => (* + 1)).WHAT  # was (Pair)   now (WhateverCode)
```

The practical fallout was that passing such an expression to `.map` / `.grep`
failed, because the argument was a `Pair` value rather than a callable:

```raku
my %h = (str => 1, int => 2);
%h.kv.map("ns:" ~ * => *);
# was:  X::Cannot::Map: Cannot map a Pair to a Seq
# now:  (ns:str => 1, ns:int => 2)
```

## What was wrong

The six `=>` handlers in `src/parser/expr/mod.rs` (`expression`,
`expression_no_assign`, `expression_no_word_logical`, `expression_no_sequence`,
`listop_arg_expr`, `call_arg_expr`) each built the pair node and returned it
**before** the Whatever-curry-wrapping step that the rest of `expression()` runs.
So a `*` under a fat-arrow was never inspected for currying and the node was
emitted as a literal `Pair`.

## The fix

All six sites now route the constructed pair through a shared
`fat_arrow_result(is_bareword, pair)` helper, which:

- keeps a **bareword** key as a named-argument `Pair` (`a => *` stays a `Pair`,
  matching raku);
- otherwise consults a new `fat_arrow_curries(left, right)` predicate and, when
  it fires, wraps the pair into a `WhateverCode` via the existing
  `wrap_whatevercode` machinery; else builds the `PositionalPair` as before.

`fat_arrow_curries` reuses `should_wrap_whatevercode` for the compound cases (so
`xx`, `o`, smartmatch and friends still opt out — `* xx 3 => 1` stays a `Pair`)
and additionally treats a bare `*` or an already-wrapped `(* … )` operand as a
curry trigger. The decision is made at the construction site rather than in
`contains_whatever` because a colonpair (`:as(*)`) and a string-keyed `=>` pair
(`"as" => *`) share the same inner `Binary{FatArrow, Literal(Str), …}` AST and
are only distinguishable by their caller; the colonpair exemption in
`contains_whatever` is left intact, so `:as(*)` and `:foo(* + 1)` stay literal
`Pair`s exactly as before.

Verified against `raku` across the full currying table (12 forms). Pin:
`t/whatever-curry-fatarrow.t`.

## Why it mattered

This was the module-load blocker for the `YAMLish` YAML battery candidate
(`docs/batteries/yaml.md`), whose `flatten-tags` runs `... $^namespace ~ * => *`
at `use` time. Fixing it is a general correctness improvement — any code that
currently passes a curried fat-arrow to a higher-order routine now behaves like
raku.
