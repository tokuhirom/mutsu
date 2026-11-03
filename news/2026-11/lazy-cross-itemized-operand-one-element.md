# A lazy `X` keeps an itemized operand as one element

`(@b.item X (1..*)).head(2)` read the itemized array `@b.item` as its items
(`((1, 1), (1, 2))`) when the other operand is infinite, while the finite path
(`cross_operand_list`) already kept it whole. `lazy_cross_pipe` now builds a
one-element column for an itemized `Array`/`Seq` operand
(`(($[1, 2, 3], 1), ($[1, 2, 3], 2))`, as rakudo). Pinned by
`t/collections/lazy-seq/lazy-cross-itemized-operand-one-element.t`.

Not done: the rows of a `lazy`-marked product (`(lazy gather {...}) X (3,4)`) are
itemized in rakudo (`$(1, 3)`), visible in the whole Seq's `.raku`; filed
separately.
