# A `take` in a loop body's LEAVE/KEEP phaser suspends the gather

`gather for 1..* -> $v { LEAVE { take $v } }` never returned its first
element: a `for` body with phasers runs as an inlined bare block, and the
inlined block's routine frame counted as a nested routine call, so the pull's
take-limit was parked for a loop-iteration boundary that a `for` loop never
polls. The lazy-pull depth now ignores inlined block frames, so the take
suspends the coroutine at once (annotations' `Buffer.STAMP`, #15296).
