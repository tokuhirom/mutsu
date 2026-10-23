# A multi candidate's `where` sees its module's file-level constants

A `multi` candidate (an exported multi sub, or a role `multi method` composed into
a class of the importing unit) whose `where` clause named a file-level `my constant`
of its own module died with `Cannot resolve caller` when it was matched from
another compunit. Candidate matching runs the `where` chunk before any frame of the
candidate exists, so the running frame was the caller's and the constant was looked
up in the caller's compunit. Matching now records the candidate's compunit and
routine-stack depth (`LexicalState::candidate_scope_unit`, set by
`with_candidate_unit`), and the unit-lexical and module-scope probes use the
candidate's lexical package while no deeper frame has been pushed. Both the
sub-dispatch and method-dispatch matchers use it. `Terminal::Print` 0.978's
`t/20-boxdrawing.t` now passes 35/35 like Rakudo. Fixes #14660.
