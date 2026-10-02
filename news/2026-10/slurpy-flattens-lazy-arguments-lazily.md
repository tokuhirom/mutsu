# A `*@` slurpy flattens lazy arguments lazily

Rakudo's `*@` slurpy flattens its arguments lazily. mutsu used to bind a
genuinely lazy argument as one nested element whenever another argument came
with it, so `sub g(*@x) { @x[^3] }; g(0, (1, 3 ... *))` answered
`(0 (...) (Any))` instead of `(0 1 3)` (#10888). This covered an infinite
`...` sequence, a lazy `.map` pipe and an unbounded Range, passed as is or
slipped with `|`. An unbounded Range was also cut to a 100000-element prefix.

Now each such argument becomes a lazy part of the slurpy's own sequence. That
is the same lazy concatenation a list literal builds for `0, |(1 ... *)`, so the
slurpy is `.is-lazy` and reifies only what is read.

Front mutation of such an array works too. `shift`, `unshift`, `prepend` and
`splice` on a lazy `.map`/`gather` array (or a lazy concatenation) used to die
with "No such method 'shift'". Now they keep it lazy: the mutated prefix is
followed by the source with its first elements skipped. Repeated mutations
collapse into one prefix and one skip rather than nesting, and `.skip(n)` now
jumps its whole skipped run in one indexed pull.
