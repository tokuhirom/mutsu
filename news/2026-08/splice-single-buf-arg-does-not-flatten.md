# A single `Buf`/`Blob` replacement argument to `.splice` now flattens

Found while fixing `.splice`'s one-arg rule (see
`news/2026-08/splice-replacement-arg-one-arg-rule.md`), by sweeping every
`Positional`-ish single argument against real `raku`. `Buf` was the one kind
still disagreeing: `my @a = 1,2,3; @a.splice(1,1,Buf.new(1,2))` gave
`[1, Buf.new(1,2), 3]` where `raku` gives `[1, 1, 2, 3]`.

## Root cause

`Blob` does `Positional`, so a lone `Buf` argument binds Rakudo's `(..., @new)`
`splice` candidate and contributes its elements, exactly like a lone
`Array`/`List`/`Seq`/`Range` does.

mutsu's shared helper `flatten_splice_replacement_args`
(`src/runtime/mod.rs`) implemented the one-arg rule over the list-shaped
`ValueView` variants only. A buffer is not one of those: it arrives as a
`ValueView::Instance` whose elements live in a storage node, and
`crate::runtime::utils::value_to_list` deliberately keeps it whole (list
*assignment*, `my @a = $buf`, really is one element). So the buffer fell to the
`_ =>` arm and was inserted as a single element.

## Fix

`flatten_splice_replacement_args` gained an `Instance` arm, guarded by `single`
like the other `Positional` arms, that decodes the argument through
`Interpreter::buf_as_byte_items` — the existing accessor the iterating list
methods (`map`/`grep`/`first`) already use to expand a buffer into its bytes.
Anything that is not buffer-shaped comes back `None` from that accessor and is
pushed whole, so no other instance type changed behaviour, and `value_to_list`
was left alone (its "a buffer is one element" contract is what list assignment
needs).

Verified against `raku` v2026.06 for `Buf`, `Blob` and `utf8`, with a lone `Str`
kept as one element as the control. Pinned by `t/buf-and-list-mutators.t`.
