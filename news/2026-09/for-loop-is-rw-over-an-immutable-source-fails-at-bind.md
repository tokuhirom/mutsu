# A `for` loop's `is rw` parameter over an immutable source now fails at bind time

`for (1, 2) -> $v is rw { $v = 5 }` used to run to completion and quietly
discard every write. Raku does not run it at all: the parameter binds the item
the iterator yields, an immutable source yields a value with no container behind
it, and the **bind** fails before the body is entered.

mutsu now raises the same failure, with raku's own exception and wording:

```
Parameter '$v' expects a writable container (variable) as an argument,
but got '1' (Int) as a value without a container.
```

It is a real `X::Parameter::RW` carrying `.symbol` (`$v`) and `.got` (the
offending item), not a message-only stand-in, and it fires whether or not the
body ever assigns — an empty body dies too, exactly as raku's does. This closes
rows 19 and 30 of ADR-0045's divergence table; `t/for-loop-element-alias.t`
pins them.

## The gate is the source, not the promotion

The tempting implementation is "ADR-0045 did not promote this element to a
container, so reject the `is rw` bind". That is wrong, and it is wrong in the
expensive direction. `for flat(@a) -> $v is rw` and `for @a[0, 1] -> $v is rw`
also fail to promote today — those producers have not been routed through the
element-container layer yet — but raku aliases through both and mutates the
array. Rejecting there would have swapped a lost write for a spurious death,
which is a worse answer than the bug being fixed.

So the rejection keys off the compiler's `ForLoopSpec::source_items_are_bare`
instead: a flag that already existed to mark the topic read-only, and that
answers `true` only for shapes which can never produce a container — a literal
list (`for 1, 2`), a word list (`for <a b>`), `%h.keys`, and now any `Range`,
whatever its endpoints are (`for $a .. $b` is as immutable as `for 1 .. 2`).
Sources raku also rejects but the flag does not yet recognise — `@a.map(...)`,
`.List`, `.Seq`, a sub's returned list, a `Hash`'s Pairs — keep the old silent
behaviour. Widening the flag later is purely additive.

A sigilless `-> \v` parameter is deliberately exempt: raku binds it to a bare
item happily and only dies if the body *assigns* through it ("Cannot modify an
immutable Int"). The AST stores `\v` as the plain name `"v"`, so the parameter
name cannot carry that distinction — `ForLoopSpec` gained a `param_sigilless`
flag for it.

## The routine-call form got the same wording for free

mutsu had an invented message for the same exception on the routine path:
`sub f($x is rw) {}; f(1)` reported `X::Parameter::RW: 'x' expects a writable
variable argument` — no sigil, no `.symbol`/`.got`, and not an exception
instance at all, only a class name spelled into the message text. Both
signature-binder sites now build the shared
`RuntimeError::parameter_rw_not_container`, so the routine form matches raku
too, and the arm in `runtime/calls.rs` that must not rewrap this error accepts
the typed instance alongside the old message convention.

Verified with `make test`, a 454-file targeted roast sweep over every
whitelisted file mentioning `is rw` / `is raw` / `<->` or a pointy `for`, and
the bundled-battery gate.
