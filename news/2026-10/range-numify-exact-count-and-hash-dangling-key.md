# Exact Range numification and sequential `.Hash` pairing

`Int - ^N` (and any Range in numeric context) now yields the exact element count
instead of one capped at the 1,000,000-element expansion limit, including ranges with
a BigInt endpoint. This is what Data::MessagePack's `$v -^ $mask - 1` negative-integer
decode relies on.

`.Hash` / `%h = ...` now decides "odd number of elements" by consuming items left to
right like Rakudo: a Hash in value position is an ordinary value, so
`('c', {x => 1}).Hash` is `{c => {x => 1}}` instead of dying.

With both fixes all 26 Data::MessagePack test files pass under mutsu.
