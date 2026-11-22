# Regex: a backreference in a repeated group reads its own iteration's capture

`"bookkeeper" ~~ /[ (.) $0 ]+/` matched only `oo`, and `[ (.) $0 ]**2`
matched `eepe`. In the second iteration, `$0` still named the first
iteration's capture. That is because an unseparated quantifier's iterations
append their captures to one flat list, and only a body that runs code was
given a per-iteration view of them. A body that holds a backreference now gets
that view too, so the backreference finds the iteration's own capture first,
as in rakudo (`ookkee`). After the quantifier, `$0` is still every iteration's
capture in a row (`/(.)+ $0/` matches `abab`) (#18736).
