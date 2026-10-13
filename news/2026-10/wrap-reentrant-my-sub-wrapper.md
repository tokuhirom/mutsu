# A `my sub` wrapper re-entered through `callsame` keeps its counter

`&f.wrap(my sub (|c) { $n++; callsame })` on a self-recursive `f` counted 1
where rakudo counts 4. Each nested entry read `$n` as 0, so `$n` ended at 1.

A closure literal written as a call argument is compiled in an escaping
position, so the variables it writes become shared cells. A `my sub (...)`
or `my method (...)` argument parses to a `do` around the anonymous routine,
and `is_closure_literal_arg` did not look inside that `do`. The closure then
captured `$n` by value. It now looks through the `do`, and the wrapper shares
`$n` with every nested entry. The same applies to Grammar::Extractor's
`my method` wrappers around a recursive token.

Closes #12803.
