# A `react` over a plain `Supplier` no longer replays what was emitted before it tapped

A live `Supplier` drops what is emitted while nobody has tapped it. mutsu kept every emitted
value in the supplier's buffer and handed all of it, minus what an earlier finished react had
consumed, to the first sink a later `react` registered:
`$sup.emit(1); $sup.emit(2); react { whenever $sup.Supply -> $v { ... }; $sup.emit(3) }` saw
`1 2 3` where rakudo sees `3`. Each react's setup hold now records the buffer length at its
`whenever`, and its sink replays from there, so only what a producer emits between the tap and
the sink registration (the race the buffer exists for, PLAN.md 8.19) is still delivered.
Red's `t/43-prefetch` (`start react whenever Red.events { flunk if $++ }`, started after a
hundred row events) hit this.
