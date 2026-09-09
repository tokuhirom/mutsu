# A `*-1` subscript only resolved when it was the last level of a chain

`@a[*-1]<x> = 'A'` silently did nothing. So did `@b[*-1][0] = 9`,
`@a[*-1]<h><y> = 'A'`, `%m<k>[*-1]<x> = 'M'`, `@c[*-1]<x>++` and
`@d[*-1]<n> += 5`. Reading through the same shape (`@a[*-1]<y>`) was always
correct, and so was a whatever subscript at the *end* of a chain
(`@a[0][*-1] = 9`, `%h<k>[*-1] = 9`) — which is why the gap survived this long.

## Root cause

A `*-1`-style subscript is a `WhateverCode`: it has to be evaluated against the
length of the container it indexes before it means anything. The chained-index
assignment ops resolved it for the **outermost** subscript only.
`exec_index_assign_expr_nested_op` had the resolution for its `outer_idx` and
nothing for its `inner_idx`; `exec_index_assign_deep_nested_op` (3+ levels) had
none at all, stringifying every index up front with

```rust
let indices: Vec<String> = indices_val.iter().map(|v| v.to_string_value()).collect();
```

`to_string_value` on an unresolved `WhateverCode` yields a non-numeric key, so
the walk autovivified a garbage slot, wrote into it, and a later read of the real
element saw `Any`. Nothing threw — the write was simply lost.

The fix resolves the index at every level, against the container *that* level
indexes. For the two-level op that container is the root variable itself. For the
deep op it is only known by walking down to it, so the resolution runs as a
read-only pre-pass over the chain **before** the existing raw-pointer walk
starts: evaluating a `WhateverCode` runs its body, and that must not happen while
a `*mut Value` into the environment is live — the resolver takes the environment
out from under it (`std::mem::take(self.env_mut())`), which would leave the
pointer dangling. A level that does not exist yet resolves against length 0,
which is what its autovivified empty container would give anyway.

## Where it was found

Reducing `Template::Jinja2` for
[#7553](https://github.com/tokuhirom/mutsu/issues/7553). Its `Context` class
keeps a stack of scopes in an array attribute and pushes one per `{% for %}`
iteration:

```raku
method set(Str:D $name, $value) { @!scope-stack[*-1]{$name} = $value }
```

Every loop variable binding therefore vanished, and `{% for x in items %}[{{ x }}]{% endfor %}`
rendered `[][][]` — the right number of iterations with nothing in them. That is
the shape the survey's "first failure" line pointed at; as usual it was a general
bug in a subsystem the template machinery never mentions.

The dist went from 3/24 to 8/24 on this fix alone (raku: 23/24).

Pin: `t/whatever-index-in-subscript-chain.t`, 18 assertions, passing unchanged
under rakudo.
