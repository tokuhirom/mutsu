# `.sort(*)` throws X::Multi::NoMatch

A bare `*` is not a comparator: no `sort` candidate takes a `Whatever`, so
`(3,1,2).sort(*)` (and the Array, Seq, lazy map and Range forms) now throws
`X::Multi::NoMatch` as Rakudo does, instead of ignoring the `*` and returning the list. The
single place is `sort_value_generic`, shared by every receiver. Red `t/38-equal` compares the
exception type of `Seq.sort(*)` with its result seq.
