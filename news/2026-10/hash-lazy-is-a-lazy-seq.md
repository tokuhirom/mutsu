# `Hash.lazy` and `Map.lazy` answer a lazy Seq of the pairs

`%h.lazy` returned the Hash itself (`.^name` was `Hash`, `.is-lazy` was `False`, `.raku` was
`{:a(1)}`): the `lazy` row (`method_table::collections::lazy`) treats a value it cannot list as
already lazy and hands the receiver back, and a Hash is neither a list nor a range there. A Hash
(and a `Map`, which `Hash` reaches) is now `Map`'s `list`, its pairs, wrapped in the same lazy
list an Array or a finite Range gets, so `%h.lazy` is a lazy `Seq`, `%(a => 1).lazy.raku` is
`(:a(1)).lazy.Seq`, and assigning it to an array keeps the array lazy. Pinned in
`t/collections/lazy-seq/lazy-marker-rows.t`
([#12058](https://github.com/tokuhirom/mutsu/issues/12058)).
