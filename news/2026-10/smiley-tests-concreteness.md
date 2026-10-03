# `:D`, `:U` and `.DEFINITE` test concreteness, not definedness

`sub f(--> Slip:D) { Empty }` died with "expected Slip:D but got Slip ()", and
`sub g(Any:D $x) {}; g(Failure.new)` refused the Failure as "not an object
instance". In rakudo a type smiley asks whether the value is a concrete
instance (`nqp::isconcrete`), and both `Empty` and a `Failure` are concrete,
even though `.defined` is False for them. mutsu's smiley check, `.DEFINITE` and
the native-row `value_is_definite` all used definedness.

They now use the existing `value_is_concrete`. The multi-dispatch cache keys
on the same bit, so `multi m(Any:U)` / `multi m(Any:D)` still caches per
winner, and `Empty` and the `Slip` type object, which share a type key, now
land in different buckets.

This made highlighter's `t/04-matches.rakutest` pass: its regex `matches`
candidate is declared `--> Slip:D` and returns `Empty` when nothing matched.
highlighter's `t/05-selective-importing.rakutest` still fails on #11246, a
package-less module's own exported multi missing from its `UNIT::` after the
first block-scoped `use`.
