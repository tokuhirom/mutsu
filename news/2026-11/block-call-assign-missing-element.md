# A block's missing-element tail is assignable when the call is the target

`my &f = -> $i { %h{$i} }; f("t") = 1` died `Cannot modify an immutable Package ((Any))` when
`%h{"t"}` did not exist (an existing key worked, and an `is rw` routine always did). The
block's subscript tail (`IndexTailElem`, #16919/#17094) reads a missing entry as its value,
which is right for a `.map` callback or a plain read but not when the call is an assignment
target. That op now keeps the deferred element token while `in_lvalue_assignment` is set,
so the write creates the key/index, exactly as for an `is rw` routine. A plain read still does
not autovivify. Test::Mock's `rw-methods.rakutest` (an `overriding` closure over a `state`
hash) now matches `raku`; all of its files do.
