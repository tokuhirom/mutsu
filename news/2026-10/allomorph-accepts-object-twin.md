# An allomorph matches an object through its `.Str` and `.Numeric`

`N.new ~~ <42>` was `False` for a class whose `.Str` is `"42"` and `.Numeric` is `42`, though
`<42>.ACCEPTS(N.new)` was `True`: only the method call reached the by-name gate that read the
object. That gate is now the late interpreter twin `Allomorph.ACCEPTS` (ADR-11276 §9.191), with
Rakudo's body (a `Numeric` argument is matched by the number, a `Str` by the string, anything else
by both), and smartmatch reaches it too. An instance of a `Str` subclass now smartmatches its
string (`S.new(value => "42") ~~ "42"` is `True`, as in Rakudo).
