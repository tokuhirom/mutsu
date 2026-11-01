# `@a[$i] = $v` that appends one past the end takes the compiled element-store lane

The compiled array element store (`try_fast_array_element_assign`) served an
in-range subscript only: a store with `$i == @a.elems`, the idiom for building
an array element by element, always fell through to the general
`exec_index_assign_expr_named_op_inner`, about 13,000 instructions of the
~22,000 a `my @a; for ^$n -> $i { @a[$i] = $i }` iteration costs. An append
creates no hole and needs no subscript-sized allocation, so the lane now accepts
`$i == @a.elems` and grows through `ArrayData::store_element`, the body
`ASSIGN-POS` already shares; a store that leaves a gap, a typed array, an
`is default` array and a bound array keep going the general way (the gap case
owns the bound on a subscript-sized resize). Pinned by
`t/collections/array/array-element-store-append.t`.
