# EVAL of a WhateverCode tree (ADR-0033 Phase 3)

`EVAL(Q{(1..5).map(* + 1)}.AST)` works. This was the last open phase of
[ADR-0033](../../docs/adr/0033-whatever-priming-leaf-and-derived-scope.md), and
it closes the `WhateverCode` item of `todo/deep/rakuast-remaining.md`: Phase 2
gave `* + 1` a `.AST`, but the write direction refused the tree it produced, so
the highest-frequency construct in real Raku code — `.map(* + 1)`,
`.grep(* > 3)`, `@a[* - 1]` — could be read and never lowered.

## Why it was not just a missing match arm

`RakuAST::WhateverCode::Argument` → `Expr::WhateverArg` is one line. The work
was the priming *scope*.

mutsu's parser plants a `WhateverCurry` marker at ~29 grammar positions; a tree
lowered from RakuAST has no parser behind it and therefore no scopes at all.
Re-deriving them is not a matter of applying the parser's predicate wherever it
says yes:

- **Bottom-up** — wrapping each qualifying subexpression as `lower_expr` returns
  it — gives *minimal* scopes. `*.abs + 1` becomes
  `WhateverCurry(WhateverCurry(*.abs) + 1)`, an inner closure added to `1`,
  where the parser produces one scope over the whole sum.
- **Top-down** gives maximal scopes correctly almost everywhere, because
  `contains_whatever` is deliberately *not* transparent through a call argument,
  a method-call argument, or a thunk barrier. That is what makes
  `@a.first(* > 1)` plant one scope around the argument and none around the
  method call, with no special case needed.

So the implementation is the second, run as a mode of the walk that already
exists. `whatever_curry::mark`'s post-parse walk reaches every expression slot
top-down and already calls `plant_here` before recursing; in the new mode
`plant_here` also materialises a scope around the node itself, so the *first*
node that primes gets the marker. `rakuast::lower` turns the mode on for its
single `mark_program` call. The parser path never sets it and is unchanged by
construction — which is the whole reason the mode exists rather than a change to
the default rule.

## Two adjustments, both found by differential testing

- **A marker's body must not be re-planted.** `mark_expr` recurses into a
  `WhateverCurry`'s body, which is still an expression that primes, so it was
  wrapped again — and again, until the stack overflowed. `plant_here` split into
  a scope half and a barrier half, and the marker-body recursion now runs only
  the barrier half.
- **An invocation is never itself a scope.** `should_wrap_whatevercode` answers
  `true` for a `CallOn` with a compound target only because the parser never
  asks it — the parser wraps the *target* at a dedicated site instead. Wrapping
  the whole `CallOn` made `(* + 1)(4)` evaluate to the closure instead of
  calling it.

## The oracle

mutsu against itself: for a snippet `S`, running `S` directly must produce
exactly what `EVAL(Q{S}.AST)` produces. That comparison found both adjustments
above, and it now agrees across the priming corpus.

`t/rakuast-eval-whatever-code.t` (18 assertions) pins that corpus — the
canonical `.map` / `.grep` / subscript forms, maximal-scope compounds, immediate
invocation, multi-`*` arity, the Phase 4 thunk-barrier cases, and the
value-position `*` forms that must NOT curry. It is a dual-oracle test: it
passes verbatim under both mutsu and raku.
