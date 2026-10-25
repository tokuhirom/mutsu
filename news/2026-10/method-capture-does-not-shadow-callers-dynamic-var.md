# A method's captured env no longer hides the caller's dynamic variable

A method installed through the HOW (`type.^add_multi_method: $name, method (T:U:) { ... }`, Red's
`Comparate` readers) keeps a snapshot of the scope it was defined in and installed it over the
call's env when authoritative. When that snapshot held a dynamic variable (`$*RED-INTERNAL`), it
replaced the binding the CALLER had made, so `die ... without $*RED-INTERNAL` fired inside a
`my $*RED-INTERNAL = True` block. Dynamic variables belong to the call chain, not to the declaring
scope, so the install now skips every `*`-twigil name. Surfaced by Red's `t/01-tdd`, whose anonymous
`given model { ... }` blocks (declared in a file with a file-scope `my $*RED-INTERNAL` and a later
`model` statement) got a stale snapshot; the file now runs to its 80th test.
