# `flat` keeps a lazy argument lazy when it has several arguments

`flat 1, [\*] @primes` forced the infinite scan before `flat` could return,
hanging or dying with "Cannot coerce an infinite lazy list to a strict list".
With more than one argument, a genuinely lazy list is now wrapped in a lazy
flattening pipe and the arguments are read one after another, so the result is
a lazy Seq as in Rakudo. This was the last load-time blocker found in
`Math::Sequences` (`our @A002110 is export = lazy flat 1, [\*] @A000040`),
#15328.
