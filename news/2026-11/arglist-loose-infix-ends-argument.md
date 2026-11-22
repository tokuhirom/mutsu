# A loose `and`/`or` inside a call's parentheses is a compile error

`@a.first(* > 1 and * < 5)` ran in mutsu and printed `1`; rakudo refuses it:
"Unable to parse expression in argument list; couldn't find final ')'". A
parenthesized argument list is rakudo's `arglist`, `EXPR('e=')`, whose items
stop before the loose word-logicals (`and`, `or`, `xor`, `andthen`,
`orelse`, `notandthen`), so one of them directly inside `f(...)` or
`.m(...)` leaves the closing `)` unreachable.

mutsu now parses each argument of a parenthesized call at that level and
reports the same `X::Comp::FailGoal`. Wrapping the expression in its own
parentheses (`f((a and b))`), a capture literal (`\(1 and 2)`, which rakudo
reads as full expressions) and listop calls (`f 1 and 2`, already
`f(1) and 2`) are unchanged (#18140).
