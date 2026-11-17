# A call of a `multi` resolves to its dispatcher

In the unit tree `$*CU` answers at CHECK time, a call of a `multi` routine now
resolves to its dispatcher, as in rakudo. The resolution is a `RakuAST::Sub`
whose `multiness` is `proto`: the explicit `proto` of the scope, or the one the
first candidate implies. Its `compile-time-value` is the dispatcher, and every
one of its `.candidates` is the same object (`nqp::objectid`) that the
candidate's own declaration answers. Before, the call resolved to the last
`multi` declaration, so a static analysis saw only that one candidate.

A second bug showed up on the way. In a unit with a CHECK phaser, the unit's
subs are installed before the CHECK runs, but only the `multi` candidates were
moved ahead; an explicit `proto` stayed in place. It was then registered after
its candidates and emptied their list, so every call died with "Routine does
not have any candidates". The `proto` now moves with its candidates, in source
order.

With both fixes, the `static` distribution's `t/03-calls` and
`t/09-signature-coverage` match rakudo.
