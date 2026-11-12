# `our <declarator> X is export` keeps the declarator's HOW

A package declarator registered through `EXPORTHOW::DECLARE` or a slang (EEL's
`event`, a `gizmo` from a module) written with a `my`/`our` scope and an
`is export` trait built a plain `ClassHOW` class: the export trait wraps the
declaration in a registration statement, and the scoped parse set the mark
naming the declarator only on a bare class declaration. It now reaches the
declaration through the wrapper, as the unscoped form already did.

EEL's CLI finds the events a module exports by `.^isa(Event)`, so
`our event CLIInferred is export` was invisible to it and every CLI test
(`t/12-cli.t`, `t/14-cli-merge.t`, `t/22-cli-transform.t`, `t/33-cli-eval.t`,
`t/35-candidate-identity.t`) died with "Unknown event".
