# A `Bool` argument and a `--> Nil` return no longer take the full type check

A `Bool` argument bound to a parameter typed `Bool:D`, `Int`, `Numeric`,
`Real`, `Cool`, `Any` or `Mu` used to take the whole type check: the subset,
coercion, capture and enum probes, about 2.9k instructions per parameter. An
`Int` or `Str` argument against its own core types was already answered at the
top of the check (`core_scalar_satisfies`); a `Bool` is now answered there too.
The answers are rakudo's: `True ~~ Int` holds, while `True ~~ int` and
`True ~~ Enumeration` do not, so those two still take the full check.

A method declared `--> Nil` (or `--> True`, `False`, `Empty`, `pi`,
`IterationEnd`) used to resolve its return spec on every call. That meant a
type-capture probe and interning two symbols for the memo key, only to hand
the spec back unchanged. It is now returned up front.

In callgrind instructions: a method with one `Bool:D` parameter, called 20k
times, goes from 276M to 216M. A private/public `--> Nil` pair goes from
1.99G to 1.78G. MCP::Server::Tool::Web's HTML extraction fixture drops 1.4%.
