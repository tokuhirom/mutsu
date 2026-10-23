# Regex.assuming returns a curried Sub

`/a/.assuming(...)` used to die with `No such method` although `.^can('assuming')` was
true. It now returns a `Sub` named `assumed.` that calls the regex with the assumed
arguments, like Rakudo's. Pinned in `t/regex/regex-assuming.t`.
