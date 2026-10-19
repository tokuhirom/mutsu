# Lazy List views render `.lazy`, `values` answers a List, itemized gather pipes keep their `$`

`Seq.values` (and `values` of any non-collection receiver) now answers a `List`, as Rakudo's
`Any.values` is `self.list`. The `.raku` of a lazy `List` view (`.List`, `.list`, `.cache` of a lazy
sequence) renders `(1, 2, 3).lazy` instead of `(1, 2, 3).lazy.Seq`, and a `$`-held `Seq` over a
`gather` `map`/`grep` pipe keeps its `$( ... )` when it is reified for a method call.
