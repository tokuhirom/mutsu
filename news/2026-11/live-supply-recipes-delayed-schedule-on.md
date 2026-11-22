# Live `delayed` and `schedule-on` keep the per-tap chain

`delayed` and `schedule-on` over a live per-tap recipe (`$s.Supply.map(...)
.delayed(1)`) used to instantiate the chain below them once, when the method
ran, and share it between every tap. A `map` there ran once per value
whatever the number of taps, and closing a tap released nothing.

They now mark the recipe itself. Each tap's own output supplier carries the
delay or the scheduler, so every tap delays or schedules its own instance of
the chain, as in rakudo (ADR-18443, #18579).
