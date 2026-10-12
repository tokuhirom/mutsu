# `Any`'s list methods reach every receiver through the method table

`end`, `flat`, `sort`, `reverse`, `unique` and `repeated` are declared on
`Any`. mutsu answered them on receivers without a table shape (a sub, a
promise, an enum value, a type object, a user-class instance) through
name-matching cascade arms that ran for every receiver. Those arms are gone.

Every receiver now reaches `Any`'s rows. The rows of `end` and `reverse` carry
the full bodies the arms had. A type object lists as itself, as in Rakudo
(`Str.sort` is `(Str,)` and `Int.end` is `0`). Each closed shape reaches the
subset of these rows its handlers read. A receiver with no shape reaches them
once the cascades have declined it, so a receiver-specific answer such as a
`Capture`'s `flat` still comes first. The answers are the same as before.
