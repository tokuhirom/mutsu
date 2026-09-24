# `nqp::list_i` / `_n` / `_s` grow with their native zero, `nqp::list` with null

MoarVM has typed native arrays (`nqp::list_i`, `list_n`, `list_s`) and untyped
object ones (`nqp::list`). Growing one -- `nqp::setelems`, or a `bindpos*` past
the end -- opens slots whose content follows the array's type: `0`, `0e0` and
the null string for the native kinds, null for an object array (`nqp::isnull`
answers 1). mutsu backed all four with the same plain list, so it could not tell
them apart: `nqp::setelems` filled every gap with `0`, and an `nqp::list` grown
that way read back `0` where rakudo reads null.

`ArrayData` now carries an `nqp_elem: NqpElemKind` (`Object` by default; `Int`,
`Num`, `Str` for the three typed constructors), set by `nqp::list_i`/`_n`/`_s`
and carried through clones like the rest of the container metadata.
`nqp::setelems` picks the fill from it -- the native zero for a typed list, 0
for a native `array[T]` (unchanged: CBOR::Simple presizes `array[num32].new`
this way), null for everything else -- and `nqp_backing::bind_elem` lets a typed
list's kind override the op's own fill, so `nqp::bindpos_i` past the end of an
`nqp::list_i` keeps leaving `0` gaps.

Pinned by `t/vm/nqp-list-setelems-fill.t` (expected values measured with
rakudo). Closes #9235, a leftover of ADR-0118 section 2.2 (#9225).
