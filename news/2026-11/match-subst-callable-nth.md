# `.match` / `.subst`: a Callable `:nth`

`"aabbb".match(/<[abc]>/, :nth({2}))` answered a one-element list, and
`.subst(…, :nth({2}))` (also spelled `:nd{2;}`) died "Attempt to retrieve before
:1st match -- :nth(0)". The block was read as a list on one side and numified
on the other. As in rakudo, a Callable `:nth` is now called once the number of
matches is known, and its answer stands in for it: a WhateverCode (`*-1`)
receives the count, a bare block is called with no arguments. A single index
gives `.match` a single Match, and an answer past the matches selects nothing.
Both methods resolve it through one helper (`runtime/match_nth_callable.rs`)
(#18790). The `s///` operator still reads `:nth` as text, which breaks
`s:nth($n)///`: #18816.
