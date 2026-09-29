# Apply an array default before its initializer

An array declaration now applies `is default(...)` before evaluating its
initializer. A `Nil` list item uses that default, while an `Any` already
stored by an inner array stays `Any`. Trait arguments also run before the
initializer, matching Rakudo's evaluation order.
