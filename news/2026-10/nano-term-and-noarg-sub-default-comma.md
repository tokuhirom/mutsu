# `nano` term under `use v6.*`, and no-arg private subs as defaults before a comma

Found via the `Email::MessageID` ecosystem distribution. `use v6.*` now selects the 6.e revision
(as in rakudo), and the 6.e `nano` term returns the epoch time in nanoseconds, sharing one
`epoch_nanos` routine with `nqp::time`. A user-declared no-argument sub followed by a comma
(`:$user = generate-key, :$header`) now parses as a call rather than a bareword, so a module's
lexical sub is called in its own scope when the default is evaluated for an importing caller.
