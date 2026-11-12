# `add_dispatchee` grows the dispatcher it is called on

In rakudo, `$dispatcher.add_dispatchee($candidate)` changes the dispatcher
object, so every alias of it dispatches to the new candidate. mutsu's
dispatcher value holds the candidates it captured, so a call through it never
saw an added candidate. The candidates now also live in the routine's shared
cell, and a dispatcher reads them from there. A clone of a dispatcher starts
with the same candidates and grows on its own.

A `my proto` with no candidates yet, used as a value
(`my $p := my proto f(Int) {*}`), was a by-name handle. Once the declaring
scope was gone (a module whose load was over) it lost its signature and could
not be called. It is now a dispatcher value of its own that keeps the proto's
signature and takes `add_dispatchee`.

Together these take CompUnit::Util 0.7.0's `t/09-push-multi.rakutest` past its
`push-unit-multi` and `push-lexical-multi` tests (#15371).
