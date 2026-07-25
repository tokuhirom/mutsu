# A placeholder var inside a nested WhateverCode is closed over, not mis-bound

When an explicit placeholder block `{ … $^name … }` contains a **nested**
currying `*` (one scoped to a sub-expression, such as a method-call argument),
and that sub-expression also references one of the block's `$^` placeholders,
mutsu used to die at compile time:

```
Placeholder variable '$^name' cannot override existing signature
```

```raku
say <a b>.map({ |(1, 2).map($^k ~ "-" ~ *) });
# was:  Placeholder variable '$^k' cannot override existing signature
# now:  (a-1 a-2 b-1 b-2)
```

The outer block owns `$^k`; the inner `$^k ~ "-" ~ *` curries into a
`WhateverCode` that **closes over** `$^k`. mutsu instead swept `$^k` into the
inner WhateverCode's synthetic `*`-derived signature and rejected it.

## What was wrong

A WhateverCode is synthesized from `*` and owns only its `*`-derived parameters;
per Rakudo it does **not** capture `$^name` placeholders — those belong to the
nearest enclosing *explicit* block. mutsu got this wrong in two places:

1. `collect_placeholders_shallow` (which computes a block's own placeholder
   params) stopped at every closure boundary, including a synthesized
   WhateverCode — so the outer block never saw a `$^name` that appeared only
   inside a nested WhateverCode, and its arity came out too small.
2. `compile_expr_anon_sub_params` raised `X::Signature::Placeholder` whenever a
   closure with an explicit signature contained a `$^name` in its body — and a
   WhateverCode's `*`-derived params count as an explicit signature, so a
   closed-over placeholder tripped it.

## The fix

1. `collect_placeholders_shallow` now **descends through** a WhateverCode
   (`is_whatever_code`) closure so a `$^name` inside it is attributed to the
   enclosing explicit block (it still stops at real `{ }` / `-> { }` / `sub { }`
   boundaries).
2. The "placeholder cannot override existing signature" check is **skipped for
   WhateverCode bodies**, since their `$^name`s are free variables from the
   enclosing block, not overrides of the WhateverCode's own signature.

Verified against `raku`. Pin: `t/placeholder-in-nested-whatevercode.t`.

## Why it mattered

This was the module-load blocker for the `YAMLish` YAML battery candidate
(`docs/batteries/yaml.md`) that surfaced once `=>` Whatever-currying was fixed
(`news/2026-07/whatever-curry-through-fatarrow.md`): YAMLish's `flatten-tags`
runs `%tags.kv.map({ |$^value.kv.map($^namespace ~ * => *) })` at `use` time,
which is exactly this shape. With both fixes, `use YAMLish` now loads.
