# Lock, Cancellation, Tap and Proc methods are method-table rows

The methods of `Lock`, `Lock::Async`, `Lock::ConditionVariable`, `Cancellation`, `Tap` and `Proc` are now rows of
the one method table (ADR-11276 §9.157). They used to be by-name `match` cascades in the native dispatcher, and
the hand-written native-method name lists that gated those cascades are now derived from the rows.

Some names now match Rakudo's declarations. `Lock::Async` has no `condition`. `Tap` has no `cancel` alias, and
`Proc` has no `Int` or `run`. The tap of a listening socket is an `IO::Socket::Async::ListenSocket`, a `Tap`
subclass with `socket-port` and `socket-host`.
