# `BEGIN` in a token, rule or regex body runs at BEGIN time

A `BEGIN` statement in a code block of a `token`, `rule` or `regex` declaration
used to run when the declaration was matched, because the declaration's matcher
parses the pattern text and the nested-BEGIN lift stopped at the declaration.
The lift now walks the code blocks of the declaration's regex tree and cuts the
lifted statements out of the pattern text too, so `grammar G { token TOP { a {
BEGIN @log.push('x') } b } }` pushes once, before the mainline, and never again
(#12812). A declaration with a `BEGIN` anywhere but as a statement of a plain
code block keeps the old handling.
