# `s///` on an immutable topic dies with rakudo's `X::AdHoc` wording

Found by the raku-doc differential sweep (`Language/faq.rakudoc:566`). A destructive `s///` whose
topic is read-only raised `X::Assignment::RO` "Cannot modify an immutable Str"; rakudo raises
`X::AdHoc`: "Cannot assign to an immutable value" for a literal topic and "Cannot assign to a
readonly variable or a value" for a bound `$_` parameter. mutsu now chooses the wording from the
topic's read-only kind. (Some smartmatch / `for $x` shapes still pick the other wording.)
