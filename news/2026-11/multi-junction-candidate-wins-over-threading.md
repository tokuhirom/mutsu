# A multi candidate that takes a Junction whole beats autothreading

`multi ep(Ex:D $l, Junction $j)` next to `multi ep(Ex:D $l, Any:D $e)` threaded `ep(I.new, 1|2)`
over the Any candidate, because the autothread decision looked only at the candidate chosen for an
eigenstate. The sub call path now resolves the real arguments first, so a `Junction`/`Mu` candidate
receives the Junction, as in rakudo (SQL::Abstract's `expand-partial`, #16769).
