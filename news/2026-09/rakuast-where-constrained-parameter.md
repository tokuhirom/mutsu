# RakuAST reads a where-constrained parameter

`sub f($x where * > 0)` renders its constraint. `RakuAST::Parameter.new(:where)`
has been constructible since Phase 4 slice 11 (`t/rakuast-construct-where.t`),
and `EVAL` lowers *and enforces* the constraint — only the read direction was
missing, so the one shape `.new` builds could not be obtained from source. The
converter refused the whole parameter as a "non-trivial signature parameter".

## Change

`src/rakuast/convert.rs`'s `parameter` builder emits a `where` field for a
positional parameter that has one, converting the constraint expression like any
other. It follows `optional` / `default` in the model's canonical accessor order
(`type, names, target, optional, default, where, slurpy`), which is the order
`RakuAST::Parameter.new` already renders. A where-constrained *slurpy* or *named*
parameter stays a boundary, alongside the typed forms of each that were already
deferred.

## The leaf classification it exposed

`* > 0` in a `where` clause rendered `RakuAST::Term::Whatever` where raku has
`RakuAST::WhateverCode::Argument`. ADR-0033's post-parse leaf classifier stops at
a routine's *body*, so a `*` anywhere in a signature kept the value
classification it was parsed with. That was invisible while the converter
refused where-constrained parameters outright; rendering one would have shipped
a knowingly wrong node.

`whatever_curry::mark` now also walks a routine's `param_defs` — both
expression-valued fields, a default (`$x = *`) and a `where` constraint — for
`sub`, `method`, `proto` declarations and for a closure with an explicit
signature. Per that module's own safety invariant a leaf classification is a
pure annotation, so this cannot change how a program runs; what it does change
is that the ADR-0033 Phase 4 thunk-barrier rule now reaches inside a signature
too, which is where it always should have applied
(`sub f($x where * > 0 && * < 5)`).

## Coverage

`t/rakuast-where-parameter.t` (12 assertions) pins the rendered `where` field
and its accessor, the corrected `WhateverCode::Argument` leaf (and that
`Term::Whatever` is *not* used), a block constraint, a typed constraint, that a
plain parameter emits no `where` field, and four `EVAL` round trips including
the rejection of a failing argument and a constraint spanning a `&&`. It is a
dual-oracle test: it passes verbatim under both mutsu and raku.
