# `:nth(1…3)` selects like a List; an empty multi-`:nth` match is `()`

A Seq `:nth` (`1…3`) was numified to 0 by `.subst` and `subst-mutate` ("Attempt
to retrieve before :1st match"), and with a Str matcher `subst-mutate` answered
one Match instead of a List. It now goes through the shared `:nth` resolver and
counts as selecting several matches. When nothing matches, a list-valued `:nth`
(a List, Seq or Range) now answers the empty list, as `:g` does; only a single
index answers Nil, as in rakudo. Roast `6.c/APPENDICES/A03-older-specs/01-misc.t`
now gets through its `subst-mutate` subtests up to an anonymous-container
receiver (#18824).
