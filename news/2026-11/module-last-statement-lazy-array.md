# A module whose last statement declares a lazy `@` array loads again

`unit module M; our @A is export = 0, 1, -> $a { ... @A[$k] ... } ... *;` hung (or died
with "Cannot coerce an infinite lazy list to a strict list") at `use M`: the module body's
value is its last statement, and two things drained it. The statement-value `Pop` forced any
lazy list that was not flagged as an array's, and an untyped `our @a = LIST` published the
raw initializer Seq to the package global (a second, unflagged copy) instead of the array
container `SetLocal` built. `Pop` now follows the `SinkPop` exemptions (array-context,
itemized and `.cache`d lists are not drained) and an `our @a` publishes the local's
container. (#17035)

Not fixed here: Math::Sequences' `A000311` etc. still read wrong values — the generator's
`state $n` is advanced by a probe call at module load, so the module's `@A[$k]` reads see
the sequence one step ahead (filed on the issue).
