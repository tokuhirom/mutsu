# `Routine.onlystar`, `Routine.soft` and `Regex.cando`

Follow-up of the code-class MRO work (#14477, #14716). `Routine.onlystar` answers
`True` for a `proto` whose body is `{*}` and for a built-in routine (a proto of its
candidates in Rakudo), `False` for a plain sub, a proto with a body of its own and
a regex. `Routine.soft` answers `Nil`. `Regex.cando(\capture)` answers the regex
when the capture holds exactly one positional argument and an empty list
otherwise, as Rakudo does. `Regex.assuming` is still missing (#14716 stays open
for it).
