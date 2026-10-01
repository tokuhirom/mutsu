# Assigning an array to a range slice of a native array works

`my int32 @arr = 0 xx 10; @arr[2 ..^ 4] = @o` died with "Cannot bind to a native int
array" ([#10370](https://github.com/tokuhirom/mutsu/issues/10370), reported from outside
the project), while the same assignment with a comma-list index or an element-by-element
loop worked.

The cause was in the compiler's Slice 2b element-share detection
(`element_share_bind_value` in `src/compiler/expr_closure.rs`). A plain `@aoa[i] = @row`
shares the source array by reference, so it is compiled as an internal `:=`-style bind
plus a value-share marker. The detector decided "single subscript" by AST shape and
accepted every `Expr::Binary` index — including the range operators (`..`, `..^`, `^..`,
`^..^`, and `^N`, which parses as `0 ..^ N`), the sequence operators and `xx`, all of
which produce a list of indices. On an ordinary array the runtime later re-routed the
resulting multi-index store, so the bug was invisible there; on a native array the
bind guard fired first and refused it.

Those list-producing operators are now treated as slices, so the RHS array is
distributed by value exactly as rakudo does. Pinned by
`t/collections/subscript/native-array-slice-assign-from-array.t`.
