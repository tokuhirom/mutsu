# A late tap no longer sees a finished Supplier's `quit` or a shared block's `done`

A plain `Supplier` keeps no terminal state in raku: a tap made after it quit
sees nothing. mutsu already reset a plain Supplier after `done`, but kept its
quit reason (on the supplier state and on the Supplier's own attributes), so
every later tap's `quit =>` handler fired immediately. `quit` now settles the
same way as `done`; a `Supplier::Preserving` still replays its backlog and its
quit to the next tap.

The `.share`d supply block's output (a plain Supplier in raku) likewise
dropped its done flag once its current subscribers were notified, instead of
handing `done` to every tap made after the block completed. (#10866)
