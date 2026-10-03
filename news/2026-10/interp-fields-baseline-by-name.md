# The `Interpreter` field ratchet no longer has a shared file to conflict on

The ratchet in `make check-interp-fields` (ADR-10779 D4) used to store one
number in `scripts/interp-fields-baseline.txt`: how many direct fields
`struct Interpreter` had. Every PR that moved fields into a subsystem rewrote
that one line. Any two such PRs in flight therefore conflicted, and so did
every rebase of a stacked branch.

The baseline is now a frozen list of the field names the struct had when it
was cut, and nothing rewrites it. A PR that extracts a subsystem gives its new
holder field (`async_state`, `threads`, ...) permission by adding its own file
`scripts/interp-fields.d/<subsystem>.txt`. New files never conflict.

A field allowed by neither list fails the check, and the error names the field.
An allowed name that is no longer a field is ignored. The rule itself is
unchanged: no new direct fields.
