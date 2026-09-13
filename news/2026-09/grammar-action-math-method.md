# LaTeX::Grammar action dispatch and captures match Rakudo

The generic method dispatcher coerced every `Instance` argument to `Numeric`
for methods named `log`, `exp`, or `atan2` before checking whether the receiver
defined a user method with that name. A grammar action such as
`method exp($/)` therefore received the failed `Match.Numeric` result instead
of its `Match` argument.

The coercion now applies only when ordinary user-method dispatch does not have
an override. This restores grammar action methods whose names overlap with
native numeric operations. The regression is pinned by
`t/grammar/grammar-action-math-method.t` and was found while bringing
LaTeX::Grammar 0.0.5's action suite to parity.

The same suite also exposed two independent compatibility gaps. A `Match` is a
`Capture`, but it is not `Positional`; removing the incorrect `Capture` entries
from the static `Positional` checks lets singular captures reach action methods
without being treated as lists. The regex LTM branch rank now retains a
declarative literal consumed before a nested alternation's empty bypass, so an
earlier function branch is not displaced by a later atom branch. These fixes
restore the MathJSON, MathML, AsciiMath, and WL action files to full parity.

Finally, RakuAST conversion now preserves dynamic `::(...)` names as
`Term::Name` nodes with `Name::Part::Expression` and lowers them through `EVAL`.
This lets LaTeX::Grammar's RakuAST action file run unchanged.
