# `(1, 2 ... *)[*-1]` dies with X::Cannot::Lazy

A WhateverCode subscript such as `*-1` or `0..*-2` is called with the target's
element count. For a lazy list or an infinite Range, mutsu used to compute that
count from whatever prefix happened to be reified: the 100,000-element
strict-force cap, or the `i64::MAX` end sentinel of `1..*`. So
`(1, 2 ... *)[*-1]` answered `100000` and `(1..*)[*-1]` answered
`9223372036854775807`. The positional subscript now raises the same
`X::Cannot::Lazy` ("Cannot .elems a lazy list") that `.elems` raises, before
it forces anything. This covers a WhateverCode on its own and one inside a
list index (#10781).
