# A deferred grep of any callback shape is read a prefix at a time

A `grep` whose block has a `FIRST` phaser, several parameters or a slurpy
parameter used to drain its whole source on the first read of the Seq. The grep
loop now reads multi-parameter groups through the pull feed and fires `FIRST`
once per Seq, so `(1..10).grep(-> $a, $b { ... })[0]` runs the callback over
one group only, as Rakudo does, and a short final group raises "Too few
positionals passed". A slurpy parameter no longer counts toward the number of
elements a call takes.
