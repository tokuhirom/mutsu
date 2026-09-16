# Net::Netmask: red to partial, with four general interpreter fixes

`Net::Netmask` went from `red` (0/6 baseline files, 29/461 assertions) to `partial`
(1/6 files at parity, 256/461 assertions), with `t/01-basic.t` fully green
(184/184, up from dying at assertion 26). None of the fixes are specific to this
distribution — each is a general interpreter bug the module's own test suite
happened to exercise.

## A Seq stored into a Hash/Array element lost its `Seq` identity

`Value::item()`/`itemize_for_element_store()`'s fallback for a still-deferred
`Seq` wrapped it in a `Value::scalar(...)` container. That wrapper drops the
NaN-boxed `Kind::Seq` tag, so `is_seq_value()` — the tag probe the
reify-before-stringify guard in `coerce_stringy_operand` uses — reported
`false` for a value that was still logically a `Seq`. A `%h<k> = @a.map(...)`
store (or the equivalent array-element store) then left the Seq's lazy
generation unpulled, so a later `eq`/interpolation on it silently read the
empty not-yet-reified elements instead of forcing a pull first.

Fixed by keeping the Seq's `Kind::Seq` tag through the store: it is now
retagged to `SeqView::ItemSeq` (the same handle-level itemization
`itemize_scalar_store_value` already used for a plain `$x = SEQ` assignment)
rather than boxed in a `Scalar`. The accompanying `mark_itemized()` call
exempts it from being silently consumed by a following statement's implicit
sink (`%h<k> .= unique;` discards the assignment expression's value in sink
context, and `SeqBody::sink_inner` only honors that flag). The array-element
`.raku` renderer (`raku_value_as_element`) gained a matching bare-render arm
for `SeqView::ItemSeq`, so a real-array element's Seq still prints
`[(7, 8).Seq,]` rather than `[$((7, 8).Seq),]` — the itemization is real, but
whole-array `.raku` was already documented to render every element kind
without its `$` marker.

Found via `enumerate(:nets)` (a `.map`-built Seq) round-tripped through a Hash
element and compared with the vendored `Test::is`.

## A `where` block that itself throws had its exception swallowed

Every `where`-constraint evaluator in `runtime/types/binding_signature.rs`
computed the block's truthiness with `.unwrap_or(false)`, which silently
turned a `die` *inside* the where block into a generic "constraint not met"
failure — discarding whatever message or exception type the block actually
raised. Fixed by propagating the block's own error with `?` (after restoring
the topic and any shadowed binding, so a mid-evaluation die still leaves env
consistent) instead of collapsing it to `false`.

Found via `Net::Netmask`'s `dec2ip` helper, whose `where` clause dies with
`'not in IPv4 range 0-4294967295'` on an out-of-range value — mutsu reported
a generic `X::TypeCheck::Binding::Parameter` instead of that message.

## A user `.succ`/`.pred` that throws had its exception swallowed too

`increment_value_smart`/`decrement_value_smart` used
`if let Ok(result) = self.try_compiled_method_or_interpret(...) { ... }`,
which can't tell "no such method" (must fall through to a plain numeric
increment) apart from "the method threw" (must not) — so `.next`/`.prev`
methods that die (as `Net::Netmask`'s do, via the same `dec2ip` helper above)
silently fell back to a bare `Int` decrement instead of propagating. An
initial fix pre-checked `class_has_method(class_name, "succ")` before
calling, but that check only sees the class registry — a built-in type's
`.succ` (`Date`, for instance) is dispatched natively without ever being
registered there, so the pre-check regressed `Date++` to a bare `1`
(caught by `roast/integration/advent2010-day16.t` and friends). Fixed by
trying the call unconditionally and reading the *returned error's shape*
instead: `RuntimeError::is_method_not_found_for("succ")` specifically means
"the invocant has no such method", as opposed to some other failure the
method body raised.

## `(start..end)[@n]` panicked instead of answering Nil past i64::MAX

Net::Netmask's IPv6 support computes network addresses well beyond
`i64::MAX` (IPv6 goes up to 2**128-1). Indexing a Range by an array of
positions computed `start + i` with a raw `+`, which panics
("attempt to add with overflow") once the arithmetic doesn't fit an `i64` —
an out-of-range index either way, so `checked_add` now answers `Nil` instead
of crashing the interpreter (`roast/S06-signature` and this distribution's
`t/06-basic6.t` both exercise `.nth`-style Range indexing at that scale).

## What's still red

Five further, unrelated interpreter gaps `Net::Netmask` exposed are filed
rather than fixed here, since each needs its own investigation or design
call: [#8584](https://github.com/tokuhirom/mutsu/issues/8584) (`sprintf`/`.fmt()`
numeric specs don't coerce a `Match`/Instance argument via `.Numeric`, which
defeats the module's own netmask-contiguity validation regex),
[#8585](https://github.com/tokuhirom/mutsu/issues/8585) (`::` IPv6
abbreviation fails to parse — a numbered-capture regex-engine gap),
[#8586](https://github.com/tokuhirom/mutsu/issues/8586) (`.sort({ block })`
doesn't reorder when the comparator's `<=>` relies on a class's implicit
`.Numeric` coercion), [#8587](https://github.com/tokuhirom/mutsu/issues/8587)
(`.flatmap` doesn't flatten a `Range` element before mapping), and
[#8588](https://github.com/tokuhirom/mutsu/issues/8588) (Range-index
arithmetic needs a `BigInt` fallback past `i64::MAX`, so an IPv6-scale
`.nth` answers a wrong value rather than crashing).
