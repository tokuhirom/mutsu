# A deferred map with FIRST/LAST phasers is pulled a bit at a time

A `.map` block that carries a `FIRST` or `LAST` phaser used to make the whole Seq be
read as soon as any element was asked for, because the loops fired those phasers once
per loop run. The phasers are now tracked per Seq (`MapGrepPlanSlot::take_first_phaser`
and `take_last_phaser`): `FIRST` fires once, before the first element, and `LAST` fires
only when a read sees the end of the source (or the block says `last`), so a prefix
read runs the callback only as far as it needs, as Rakudo does. A `grep` with such a
phaser, a multi-parameter `grep` and a chained pull are unchanged (still on #14196).
