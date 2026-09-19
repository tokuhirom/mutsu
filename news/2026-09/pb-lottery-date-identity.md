# PB-Lottery reaches parity through native temporal identity

`PB-Lottery` 0.0.1 was measured with 6 of 8 baseline files passing. Its two
remaining files compared separately constructed `Date` values with `===`, and
mutsu treated them as different objects even though Rakudo gives equal dates
the same `.WHICH` identity.

mutsu now uses the Modified Julian daycount for `Date` identity and the
canonical timestamp for `DateTime` identity, both for `.WHICH` and strict
identity comparison. `PB-Lottery` is now green at 8 of 8 baseline files and
131 of 131 assertions.

Pinned by `t/vm/identity-eq.t`, including direct `Date` and `DateTime` `.WHICH`
checks and equal-value identity comparisons.
