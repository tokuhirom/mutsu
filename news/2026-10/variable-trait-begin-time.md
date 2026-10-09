# User variable traits apply at BEGIN time

A `trait_mod:<is>(Variable ...)` handler now runs in the unit's BEGIN prologue,
in source order with the `BEGIN` blocks around the declaration, instead of when
the declaration executes. `my $attribute is env` inside two closures, each
preceded by a `BEGIN` that sets `%*ENV`, now sees its own `BEGIN`'s state
(`Here`, then `There`), as on Rakudo (#12278).

The declaration keeps only its static half and starts from the variable's
static cell on every entry. Scalar, initializer-less declarations in a nested
scope with a user trait are lifted; builtin traits, `@`/`%` containers and
declarations with an initializer keep their run-time application.
