# A `.map` block's `FIRST` phaser fires once per closure

A `FIRST` phaser in a `.map` / `.grep` block now belongs to the closure, as
on rakudo. It fires on the first call of that closure, however many maps call
it later, so `my $b = { FIRST ...; $_ }; (^2).map($b) for ^3` runs it once,
not three times. A block literal that is re-created on each pass of an
enclosing loop is a fresh closure, so it fires once per pass. A map over an
empty list never calls the block and leaves its `FIRST` for the next call.

The "already fired" flag is stored per closure clone in the same cross-thread
store that `once` blocks use.
