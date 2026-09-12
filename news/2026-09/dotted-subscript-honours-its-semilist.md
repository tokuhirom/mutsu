# The dotted subscript `.[$i; $j]` honours its semilist, and `.<>` parses

A subscript holds a **semilist**, so `@a[$i; $j]` is a multi-dimensional index.
The bracket spelling knew that; the dotted postfix spelling did not, and the
failure mode was worse than a parse error — it silently answered with the wrong
value:

```
$ raku  -e 'my $c = [[1,2],[3,4]]; say $c.[0; 1];'
2
$ mutsu -e 'my $c = [[1,2],[3,4]]; say $c.[0; 1];'
([1 2] [3 4])
```

`.[...]` and `.{...}` called `parse_bracket_indices`, the *wrapper* that exists
for callers which cannot represent dimensions: it flattens them into a single
`ArrayLiteral`. So `$c.[0; 1]` parsed as the slice `$c[(0, 1)]` — hence the
whole container back — and `%h.{"a"; "b"}` as `%h{("a", "b")}`. Both dotted arms
now go through `parse_bracket_indices_inner` and build the same `MultiDimIndex`
the bracket loop builds, lowering a single remaining dimension back to a plain
`Expr::Index`. The shared lowering lives in one helper
(`dotted_subscript_expr`) so the two arms cannot drift apart again.

The second half was a missing postfix entirely. `Game::Entities` — the
distribution that motivated the ticket — writes `.[COMPONENTS; $i].<>`, and
`.<>`, the dotted spelling of the decontainerizing zen angle subscript, had no
arm at all: the `.<key>` arm requires a non-empty key, so the empty `<>` fell
through every dotted postfix to a bare `Confused`. The issue attributed that
parse failure to the leftover semilist, but it reproduces on its own
(`my $x = 5; say $x.<>`) and is independent — the undotted `$x<>` has worked all
along. It now lowers exactly as the undotted arm does.

With both halves, `Game::Entities` 0.1.6 loads under mutsu, so its `ecosystem/`
record leaves `blocked_load` at the next sweep. Its own test suite is still red
on unrelated gaps, which belong to the `ecosystem-dist-fix` workflow.

One divergence found alongside is deliberately *not* fixed here, because it
lives in the bracket form too and predates this change: a `WhateverCode`
dimension (`@m[*-1; 0]`) returns the bare element where rakudo returns a
one-element `List`. Filed as
[#8188](https://github.com/tokuhirom/mutsu/issues/8188); the new test does not
assert that case, so the two spellings are now consistently wrong there rather
than differently wrong.

Pinned by `t/collections/subscript/dotted-subscript-semilist.t` — 21 assertions
measured against rakudo, covering two and three dimensions in both the
positional and associative spellings, the single-dimension and comma-slice
non-regressions, a trailing `;`, assignment through the subscript, `:exists`,
`.<>` on a scalar/array/hash/subscript, and the distribution's own
`.[$i; $j].<>` chain.

Closes [#8155](https://github.com/tokuhirom/mutsu/issues/8155).
