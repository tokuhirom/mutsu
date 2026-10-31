# Every method frame shape skips its plain scalar parameters in the env

The previous slice stopped writing a plain scalar parameter into the frame env
for the pure method shape (no closures, no env writes). A method with a closure
or an attribute write took the other branch and still inserted every bound
parameter by name. That branch reads a plain scalar parameter through its slot
too, and a closure captures it from the slot, so the same predicate now applies
to both: a parameter stays in the env only if it is an aggregate, attributive,
named, slurpy, sigilless, invocant, `is rw`/`is copy`/`is raw`, or its name does
not start with a letter (`$/`, `$_`, `$!`), if the body looks lexicals up by
string, or if the env already holds the name.

An ablation with the predicate of the previous slice failed two files, both
`$/` parameters (`t/regex/match/match-named-capture-sugar.t`,
`slash-param-survives-smartmatch.t`); the letter test is what keeps them.
On a method with a closure and a method that writes an attribute, 100,000 calls
each, instructions went from 6,615.9M to 6,571.3M (about 220 per call, second
callgrind run of each, one scalar parameter per method).
