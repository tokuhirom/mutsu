# A role candidate answers for its own attributes and methods

`R.^candidates[0].^get_attribute_for_usage('$!payload')` died with
`No $!payload attribute in R`, because the attribute table behind it only read
the class registry. Config::BINDish asks exactly that at load time.

A role or one of its candidates now answers `.^get_attribute_for_usage`,
`.^attribute_table`, `.^attributes` and `.^methods` from the role body's own
declarations. A role group's candidates no longer collapse into the role
registered under the bare name. A `.^` call on a candidate keeps the
candidate object as its receiver instead of turning it into the role's type
object, so `role P[::T] { has T $.typed }` and `role P { has $.plain }` each
report their own attribute.
