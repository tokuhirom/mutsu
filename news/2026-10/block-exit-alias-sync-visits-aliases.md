# A block's exit re-syncs only the locals that have a `:=` alias

A bare block's exit re-syncs each `:=` alias (`my $a := $b`) with the local the
block modified. Once any alias had been bound anywhere in the process, the exit
asked every local of the frame whether it had one: O(L) per block exit, L =
frame locals (#9170).

The `:=` alias keys that have ever been an env key are now logged, in the order
they first became one, at the same closed set of sites that latch every env key.
Each chunk indexes its slots by alias key. A block's exit visits only the slots
of the logged keys: O(a), a = aliases the program has bound, independent of the
frame's size.
