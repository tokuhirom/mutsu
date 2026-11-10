# `$list.map(&callback)` makes the callback's `$_` read-only for an immutable List

`(2,3).map(&f)` where `&f = { --$_ }` now dies like rakudo. The deferred `.map`
Seq records that its source is an immutable `List` (`MapGrepPlanSlot::immutable_source`),
and the inline loop plan ORs it into `immutable_topic`, so a callback that arrives
as a value or a raw parameter gets the same read-only topic as a literal block.
`@a.map(&f)`, `@a.list.map(&f)` and `@a.Seq.map(&f)` still write through.
Iter::Able's `t/29-for-each` now matches rakudo (all 33 files identical).
