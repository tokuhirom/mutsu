# A placeholder var inside a nested WhateverCode is mis-collected as its parameter

When an explicit placeholder block `{ … $^x … }` contains a **nested** currying
`*` (one scoped to a sub-expression, e.g. a method-call argument), and that same
sub-expression also references one of the block's `$^` placeholders, mutsu tries
to make the placeholder a parameter of the *inner* WhateverCode and dies:

```
Placeholder variable '$^namespace' cannot override existing signature
```

## Minimal repro

```raku
my %tags = ( "ns:" => { str => 1, int => 2 } );
%tags.kv.map({ |$^value.kv.map($^namespace ~ * => *) });
# raku:  [ns:str => 1 ns:int => 2]
# mutsu: Placeholder variable '$^namespace' cannot override existing signature
```

The outer block `{ |$^value.kv.map($^namespace ~ * => *) }` has placeholders
`$^value` and `$^namespace` — those belong to the outer block. The inner
`$^namespace ~ * => *` curries into a `WhateverCode` (arity 2, from the two `*`)
that **closes over** `$^namespace`. mutsu instead sweeps `$^namespace` into the
inner WhateverCode's placeholder signature, which already has its `*`-derived
params — hence "cannot override existing signature".

## Why now

This surfaced only after `=>` Whatever-currying was fixed
(`news/2026-07/whatever-curry-through-fatarrow.md` /
`t/whatever-curry-fatarrow.t`). Before that fix `$^namespace ~ * => *` was a
literal `Pair`, so no inner WhateverCode was built and no placeholder collision
occurred (the failure was the earlier `X::Cannot::Map` instead). The currying is
now correct; the remaining bug is placeholder **scoping** across a WhateverCode
boundary.

Note: a bare `*` at the *top level* of a placeholder block (`{ $^x ~ * }`) is a
**compile error in raku** ("Malformed double closure"), which mutsu does not
diagnose — that is a separate, lower-priority divergence. This ticket is only
about a `*` nested inside a sub-expression, which raku accepts.

## Root cause / affected area

The placeholder-collection pass (the code that gathers `$^name` twigil vars into
a block's implicit signature) must stop at a nested WhateverCode boundary: a
`$^name` occurring inside an already-wrapped WhateverCode body belongs to the
nearest **enclosing explicit** placeholder block, not to the WhateverCode. Look
at where placeholder params are gathered for `{ … }` blocks and where
`wrap_whatevercode` builds the inner lambda (`src/parser/expr/whatever_wrap.rs`,
`src/parser/expr/whatever_replace.rs`), and ensure the collection treats the
wrapped lambda's body as opaque w.r.t. the outer block's placeholders.

## Impact

This is blocker #1.5 for the `YAMLish` battery candidate
(`docs/batteries/yaml.md`): its module-load `flatten-tags` is exactly this
shape, so `use YAMLish` still fails here even after the `=>` currying fix (it now
fails with this error instead of `X::Cannot::Map`). Blocker #2
(`todo/tickets/yamlish-grammar-parse-dispatch.md`) is further down the same load
path.
