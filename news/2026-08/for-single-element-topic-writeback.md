# `for @a[i] { ... }` did not alias the element for writeback

`for EXPR { ... }` over a single non-iterable lvalue element (`for @a[i] {
.=Int }`) should topicalize `@a[i]` the same way `with @a[i] { ... }` does — as
an lvalue alias, so `.=Int` / `$_ = ...` inside the body write back to the
element. `raku` does this; mutsu did not:

```
$ mutsu -e 'my @a = "1","2"; for @a[1] { .=Int }; say @a.raku'
["1", "2"]     # raku: ["1", 2]
```

Found as a side note while fixing the `with`-statement-modifier element
writeback bug (`with-statement-modifier-element-writeback.md`) — that fix only
covered `with`/`given`'s `Expr::DoStmt(Stmt::Given)` compile path
(`src/compiler/expr_block.rs`), not `for`'s loop compilation
(`src/compiler/stmt.rs`), which is a different opcode family entirely and does
not go through `given`'s `TagElementSource`/`element_source` machinery at all.

## Fix

Rather than teaching `for`'s (very different) loop-execution machinery a new
`TagElementSource`-style aliasing mode, the fix is a compile-time desugaring
sibling to the existing `for %h<k>.values { ... }` element-source rewrite
(`desugar_for_element_source`): a bare var-rooted `Index` used *directly* as
the loop source (no `.values` wrapper) is rewritten from

```raku
for <ELEM> { BODY }
```

into

```raku
my $tmp = <ELEM>;
for $tmp { BODY }
<ELEM> = $tmp;
```

`for` over a bare scalar variable already writes `$_`'s final value back to
that variable (verified: `for $x { .=Int }` already updated `$x`'s type), so
routing an element source through a temp variable needed no new VM machinery —
only the new `desugar_for_scalar_element_source` compiler function
(`src/compiler/stmt.rs`), hooked in right after the existing `.values` sibling
in `Stmt::For` compilation.

Bonus fix noticed along the way: `for @a[i] { ... }` where `@a[i]` holds a
nested `Array` used to incorrectly flatten it into multiple iterations
(`for @a[0] { .say }` printed each inner element on its own line); `raku`
topicalizes the whole `Array` as one item, matching plain scalar-variable `for`
semantics. The temp-variable rewrite fixes this for free, since `for $tmp`
(over the same nested Array, now itemized into a scalar) already had the
correct non-flattening behavior.

Pin: `t/for-single-element-topic-writeback.t`.
