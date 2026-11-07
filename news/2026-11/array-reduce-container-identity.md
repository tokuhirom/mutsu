# `@array.reduce(&[=:=])` compares the elements' containers

`[$x, $y].reduce(&[=:=])` answered `True` for equal values, because `reduce` flattened the
Array to plain values. An Array's elements are its own Scalar containers, so two positions are
one container only when they share a cell; `reduce` with the builtin `=:=` / `!=:=` now decides
the identity chain on the cells (Iter::Able `is-all-same(:with(&[=:=]))`).
