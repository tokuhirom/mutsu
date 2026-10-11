# Multi sub dispatch no longer counts a slurpy as an optional positional

The optional-positional tie-break of multi **sub** dispatch counted a
slurpy as an optional positional and only zeroed the count for candidates
that declare a named parameter. So `multi j($a?, $c?, *%h where .elems == 0)`
lost to a later `multi j($b?, :$x)` for `j(1)`, where `raku` finds them tied
and runs the first declared (#12774).

Sub and method dispatch now share one rule (`multi_rank::optional_positional_count`):
slurpies are left to the non-slurpy tier that ranks before it, and a
candidate that needs a named bind check — an explicit named parameter, or a
`where` on a slurpy — counts nothing.
