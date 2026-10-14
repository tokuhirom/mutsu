# A user infix that ties the core candidate is an ambiguous call

`multi infix:<%>(Int $a, Int $b) { ... }` ties the core `(Int:D, Int:D)` candidate for
`7 % 3`. Rakudo raises `X::Multi::Ambiguous`; mutsu silently let the core operator run
for `%` and the user candidate run for `+`. The ranking of a user infix against the
modelled core set (`core_infix_candidate_wins`) now answers `User`, `Core` or `Tie`, and
an exact tie that is not `is default` raises `X::Multi::Ambiguous` for every operator
(#12973).
