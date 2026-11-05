# A smartmatch looks up a regex's variable names once per pattern

`$x ~~ / ... /` scanned the pattern text for the variables it can name on every
execution, twice (the literal and then the regex value). The names are a pure
function of the pattern text, so they are now memoized by it (bounded at 1024
distinct patterns per thread). On `bench-regex-match` this and the leading-`$var`
scan together take 459.8M instructions / 399,246 allocations to 369.8M / 335,697.
