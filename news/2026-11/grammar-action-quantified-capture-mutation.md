# A grammar action's `shift @<x>` is kept between reads again

#18616 let a grammar action read `$<name>` from the lazy Match's capture node
without building the Match. For a quantified capture it built a fresh Array
each time, so `while @<operands> { ... shift @<operands> ... }` -- C::Parser's
`conditional-expression` action -- never shortened the array and looped
forever: C::Parser's `t/02`, `t/04`, `t/05`, `t/06` and `t/07` timed out. An
Array-valued slot is a container of the Match, so it is now read from the
materialized Match, where every read reaches the same Array; a single capture
still reads lazily (#18830).
