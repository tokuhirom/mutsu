# Omitted coercion-typed optionals bind the package-qualified type object

`Column::List(Any) :$c` written inside `unit class SQ` (declared `SQ::Column::List`) bound the
unqualified `Column::List` type object when omitted, so a `Column::List $.c` attribute rejected it
("expected SQL::Abstract::Column::List but got Column::List", #16752). The omitted value now resolves
relative to the enclosing package. Regression tests also pin coerced defaults, package-relative
coercion targets called from outside the package, and `X,;` as a one-element list.
