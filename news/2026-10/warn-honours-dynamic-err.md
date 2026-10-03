# `warn` prints through the dynamic `$*ERR`

An unhandled `warn` used to write straight to the process stderr, even when
the caller had rebound `$*ERR`. So `my $*ERR = Trap.new` (the `silently`
distribution) and similar output-capture idioms missed every warning, while
`note` was captured. Rakudo's default warning handler prints through `$*ERR`,
and mutsu now does too. `quietly` and `CONTROL` handlers still see the
warning first. The `silently` test suite passes in full.
