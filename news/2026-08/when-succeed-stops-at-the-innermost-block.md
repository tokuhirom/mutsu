# A `when` succeed now stops at the innermost enclosing block

Raku unwinds the succeed signal a matched `when` raises only as far as the
**innermost block that lexically contains the `when` statement** — which is not
necessarily the enclosing `given`/`with`/`for`. An `if` branch, a bare block or
a loop body wrapped around the `when` therefore ends there, and the enclosing
topicalizer keeps running:

```raku
given 5 { if True { when Int { } }; say "after" }   # prints "after"
given 5 { { when Int { } }; say "after" }           # prints "after"
```

mutsu absorbed the succeed only in `DoBlockExpr` and in `BlockLocalScope`
(emitted when a branch declares a block-local `my`), so those two lines printed
nothing: the signal escaped to the `given` and terminated it. The same leak hit
loop bodies — `given 5 { while ... { when Int { } }; say "after" }` iterated
correctly but lost the trailing `say`, because the loops caught the succeed
without resetting the `when_matched` flag that an enclosing `given` breaks its
body on.

## The fix

A new `OpCode::SucceedBarrier { body_end }` runs a body range and absorbs a
succeed signal, truncating the stack and restoring `when_matched` — the same
treatment `exec_do_block_expr_op` already gave the `do { when ... }` form. It
adds no scoping of its own, so it costs a single dispatch and stacks cleanly on
top of whatever `BlockScope`/`BlockLocalScope` the ordinary compile path chose.

The compiler emits it exactly where the rule says the boundary is: around a body
whose *own* top-level statement list contains a `when`/`default`
(`body_has_toplevel_when`, which descends into the parser's `SyntheticBlock`
wrapper but deliberately not into nested blocks — a `when` nested one level
deeper belongs to that inner block). That check sits in
`compile_body_with_implicit_try`, covering `if`/`unless`/`else` branches and all
four loop kinds at once, plus the bare-block arm of `Stmt::Block`.

Because the barrier keys off a `when` being present, a block *without* one stays
transparent, which preserves the other half of the semantics: an explicit
`succeed` inside a plain block still travels out to the enclosing topicalizer
(`given 5 { when Int { { succeed }; say "c" }; say "d" }` exits the `given`).

Pinned by `t/when-succeed-innermost-block.t`, extended from 6 to 20 assertions
covering `if`/`elsif`/`unless`/`default` branches, nested branches, bare blocks,
`for`/`while`/C-style-`loop`/`repeat` bodies, and the explicit-`succeed`
counter-case. Every assertion passes under `raku` as well.
