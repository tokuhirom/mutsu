# A `{*}` in a method-call callback inside a proto body is `Nil`, as in Rakudo

`proto f($) { [1].map({ {*} }) }; multi f(1) { 42 }; say f(1)` printed `(*)`:
the `{*}` of a closure handed to a method stayed a block whose body is a bare
`*`, because the dispatch rewrite stopped at method-call arguments (#10555).

Measured against `raku` (2026.09), the answer is `(Nil)`, not `(42)`. Rakudo
finds a `{*}`'s dispatcher through the closure's callers, and the nearest
routine that has one is the method the closure was handed to: `map`, `first`,
`sort`, a user-defined `method`, a `multi` -- so the `{*}` evaluates to `Nil`
and never reaches the proto. A plain `sub` has no dispatcher, so a closure
handed to one (`run-sub({ {*} })`), like one the body calls itself
(`my &c = { {*} }; c()`), still dispatches. A `{*}` that is itself an argument
(`.map({*})`) is evaluated at the call like any argument: it dispatches, and its
result (`42`) is then not a callable.

That is not the fix the issue proposed (carrying the proto's dispatch context
into closures would make `.map({ {*} })` give `42`, which rakudo does not), so
the rewrite now follows what rakudo does: in `ProtoDispatch`
(`src/runtime/dispatch_proto_rewrite.rs`) a `{*}` found inside a closure that
sits in a method-call argument list becomes `Nil`, at any nesting depth, and a
`{*}` that is the argument itself becomes the dispatch call.

## Known difference

The rule is applied to closure *literals* in a method's arguments. A closure
kept in a variable first (`my &c = { {*} }; [1].map(&c)`) still dispatches in
mutsu where rakudo gives `Nil`, and a `{*}` in a routine the proto body calls is
not a dispatch point at all here: telling these apart needs the callers at run
time, not the syntax ([#10746](https://github.com/tokuhirom/mutsu/issues/10746)).

Tests: `t/routines/dispatch/proto-onlystar-positions.t` (every expectation
checked against `raku`, and the file passes under it unchanged).
