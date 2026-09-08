# A nested block's `state` restarts inside a value-position `for`

```raku
my @r = do for ^3 { { state $x = 0; $x++ } };
say @r;   # mutsu: [0 1 2]    raku: [0 0 0]
```

`state` restart semantics are decided by block *cloning*: the inner `{ ... }` is
a block literal that the loop body re-clones on every iteration, so its `state`
starts over each time. mutsu models this with a compile-time
`OpCode::ResetStateLocals` bracket rather than by actually re-cloning the block,
and `compile_bare_block_inline` is the function that emits it.

The value-collecting `for` body compiled its tail statement through
`compile_stmts_value`, whose `Stmt::Block` arm called the raw
`compile_block_inline` — which inlines the block's statements with no bracket at
all. `compile_block_inline`'s *own* tail-`Block` arm has always routed to
`compile_bare_block_inline`; the value path was simply the one place that did
not, which is why every neighbouring case already behaved. The statement-position
form (`for ^3 { { state $x = 0; ... } }`), a value-position block outside a loop
(`sub g { do { { state $x = 0; ... } } }`) and even a *doubly* nested block in a
value-position `for` were all correct, the last one because only the outermost
nested block reached the broken arm.

The arm now mirrors its sibling: a genuine source `{ ... }` in tail position goes
through `compile_bare_block_inline`, and a block carrying ENTER/LEAVE/KEEP/UNDO
phasers goes through `compile_phaser_block_scope` so the phasers still run
against a real block scope.

Getting this right had to leave the opposite case alone. A *sole-block*
statement-modifier body (`{ state $n = 0; $n++ } for 1..3`) is not a nested
block: that block **is** the loop's body, cloned once for the whole loop, so its
`state` must persist across iterations and still yields `0 1 2`. So must the loop
body's own non-nested `state`. `t/state-nested-block-value-position-for.t` pins
both directions together — three restart cases, two persist cases, and one that
a tail block carrying a `LEAVE` still runs it and still yields its value — and
the file passes unchanged on rakudo.

One neighbouring divergence is *not* fixed here and was verified to predate this
change: a nested block that carries a phaser does not restart its `state` at all
(`do for ^2 { { LEAVE { }; state $q = 0; $q++ } }` gives `[0 1]` where raku gives
`[0 0]`), because `compile_phaser_block_scope` emits no reset bracket of its own.
That function is shared with `if`/`unless` branches and several other callers,
which have their own `state` rules, so bracketing it is a wider change than this
ticket. It is filed as #7632.

Closes #7585.
