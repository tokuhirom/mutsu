# A Junction argument threads through a captured multi dispatcher

Calling a code value captured from a `proto`/`multi` family (`my &c = &compare`, or a
lexically imported multi) with a Junction argument died with "Cannot resolve caller"
whenever the call reached the family by name (the family had grown since the capture).
The by-name entry now threads the Junction one eigenstate at a time, as a direct
`compare(any(...), $x)` call does. Found via the Red distribution
(`Red::AST::Optimizer::AND`'s `has-condition` where-clauses).
