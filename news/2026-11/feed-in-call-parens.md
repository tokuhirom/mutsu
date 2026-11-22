# A feed operator inside a call's parentheses is a compile error

`f(1 ==> 2)` parsed the feed into the argument in mutsu and failed at run
time ("No such method 'CALL-ME' for invocant of type 'Int'"). A parenthesized
argument list is rakudo's `arglist`, `EXPR('e=')`, and the feeds (`==>`,
`<==`, `==>>`, `<<==`) are sequencer-level, looser than that, so -- like the
loose `and`/`or` (#18140) -- they end the argument and leave the `)`
unreachable: "Unable to parse expression in argument list; couldn't find
final ')'". mutsu now reads each parenthesized argument without the feeds
and reports the same error; a feed in its own parentheses (`f((@a ==> @b))`)
is unchanged (#18628).
