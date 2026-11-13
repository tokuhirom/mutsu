# `<$var>` interpolates a named token held in a variable

A regex's `<$var>` read a named regex/token held in the variable (`my $p =
&protocol; /<$p> '://'/`) as a string -- its routine *name* -- so the
pattern matched the literal text `protocol` and nothing else. A named
regex routine now interpolates its source, as a `Regex` value always did,
both when the pattern is parsed and when a runtime `<$var>` is matched.

`URL::Find`'s five failing test files (`find-urls`, which picks its
protocol and base tokens into variables) now pass, matching raku.
