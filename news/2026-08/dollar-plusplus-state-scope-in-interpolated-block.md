# A block's `state` restarts when its enclosing block is re-entered

`sub count-it { say "Count is {$++}" }` printed `0`, `1`, `2` on successive
calls; raku prints `0` every time. This is one of the documented traps
(`Language/traps.rakudoc`, "Using a block to interpolate anon state vars"): the
`{ … }` in a double-quoted string is its own block, and a `state` cell belongs
to a *clone* of the block that declares it, so re-entering the enclosing routine
re-clones the interpolation block and restarts the counter.

## Root cause — two independent gaps

mutsu already models a bare `$` correctly in principle: the parser mints
`__ANON_STATE_<id>__` and `take_anon_state_decls` turns it into an implicit
`state` declaration at the top of the enclosing *scope*, handing the reset
semantics to the existing state machinery.

1. **The string-interpolation block pushed no lexical scope.** The two
   double-quote scanners in `src/parser/primary/string/quoted.rs` parsed the
   block body with `stmt_list_pub` directly, so `record_anon_state_name`
   recorded the `$` against whatever scope was open — the enclosing routine
   body. The declaration was hoisted out of the block that owns it, and the
   counter then legitimately persisted for the routine's lifetime.

2. **Value-position blocks emitted no `ResetStateLocals`.** `Stmt::Block` and
   the `if`/`unless` branches already emit that opcode; `do { … }`
   (`compile_do_block_expr`), a scope-isolated block expression
   (`compile_do_block_expr_scoped`), and a routine's flattened *tail* `{ … }`
   (`compile_routine_body_stmts` / `compile_bare_block_inline`) did not. So even
   an explicit `state` counted across calls there:
   `sub f { { state $c = 0; say $c++ } }` printed `0 1 2` instead of `0 0 0`.

Both had to be fixed for the trap to reproduce; either alone leaves the counter
incrementing.

## Fix

`parse_interpolation_block` (`src/parser/primary/string/interp_content.rs`) is
the one place a `"…{ … }…"` body is parsed now: it pushes the block's own
lexical scope, parses, prepends the anonymous-`state` declarations the body
minted, and pops — so the implicit `state` lands *inside* the block. The three
value-position block compile sites emit `OpCode::ResetStateLocals` around their
body, honouring the existing sole-block loop-body suppression so a
`do { … } for @xs` body still shares its state across iterations.

`$( … )` deliberately keeps the old behaviour: it is a contextualizer, not a
block, so `"Count is $($++)"` counts `0`, `1`, … — which is exactly the
workaround the trap documentation recommends, along with `"Count is " ~ $++`.

Verified against raku: interpolated `{$++}` in a routine, at the mainline, in a
`for`/`while` body, under `xx`, inside a `map` block, an explicit `state` inside
an interpolation block, both documented workarounds, a tail bare block, a
`do { … }` block, a loop body (state shared across iterations, restarting when
the loop statement re-runs), a routine-body `$` (keeps counting), a closure
minted inside a routine, and `map { ++$ }`.

Pin: `t/regex-embedded-code-blocks.t`.
