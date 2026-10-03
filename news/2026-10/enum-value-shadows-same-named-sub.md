# A bare enum value is no longer called as a same-named sub

With `enum Unit (mins => 60)` and `sub mins(Numeric() $n)` both in scope, a
bare `mins` parsed as a zero-argument call to the sub. It died with `Calling
mins() will never work with declared signature (Numeric() $n)`. In Raku the
enum value is a term and the sub is `&mins`. A bare identifier names the term
first, whichever was declared first, so `mins`, `mins.value` and `f(3, mins)`
all reach the enum value, while `mins(2)` still calls the sub. A `constant`
already behaved this way. The parser's bare zero-argument call path and its
bareword-dot path now leave a declared enum value as a term too.

Found via the `TimeUnit` distribution, whose `timeunit(3, minutes)` passes the
`UnitTimeName::minutes` value next to an exported `sub minutes`. Its test file
now passes under mutsu (20/20).
