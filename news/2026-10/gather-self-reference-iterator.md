# A gather can read its own sequence through an iterator

`my \S := gather { my $it = S.iterator; take 1; take 2 * $it.pull-one }`
used to overflow the native stack: while the body ran, the elements it had
taken lived only in its take collector, so the inner `pull-one` saw an empty,
never-started gather and restarted the body from scratch, which re-entered it
again. The running body now records where its take collector sits on the
gather-items stack, and a pull that re-enters the same gather answers from the
elements already taken. Asking for an element the body has not taken yet
reports the cycle instead of recursing (Rakudo hangs there).

This makes the Smooth::Numbers distribution's Hamming-number generator, which
keeps one lagging iterator per prime factor over the sequence it is building,
pass its whole test suite.
