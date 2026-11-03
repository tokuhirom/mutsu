# A `does Sequence` class answers `head(n)` without draining its iterator

`class Sq does Sequence { method iterator { Endless.new } }; say Sq.new.head(3)`
hung: the role delegate drained the whole iterator into a `List` before
dispatching any method. Rakudo's `Sequence` pulls a prefix consumer lazily. The
delegate now computes how many items the call needs (`head(n)`, `head`, a bare
`first`, `AT-POS(i)`) and pulls only those with `pull-one`
(`drive_iterator_value_items_upto`); every other method still drains. A held
`has $.iterator` keeps the unread rest for a later read. Pinned by
`t/oo/role/sequence-role-prefix-pulls-lazily.t`.

Not done: `map`/`grep`/`first(matcher)` over an endless iterator still drain;
they need a lazy pipe over `pull-one`.
