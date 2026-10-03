# A `regex` caller backtracks into a proto's `regex` candidate

A proto call always entered its chosen candidate committed, so it kept only that
candidate's first end. `regex TOP { <num> '9' }` over `proto token num {*};
regex num:sym<d> { \d* }` therefore failed on `"129"`. Rakudo backtracks into
the candidate, exactly as into any other subrule, whenever the call site is not
ratcheted.

The candidate now inherits the call site's ratchet, like a plain subrule call.
Once a candidate returns, the call's choice of candidate is still settled: its
remaining ranked candidates are given up, while the candidate's own choice
points stay. A failure later in the caller therefore never moves on to another
candidate, which matches rakudo.

Found via Lingua::NumericWordForms. Its German grammar separates
`hundert-vier` with `regex preceding-number-separator:sym<German> { \h* | <:Pd> | ... }`,
where the `-` is reached only by backtracking past the empty `\h*`. The
Armenian, German and Greek parsing suites now pass in full.
