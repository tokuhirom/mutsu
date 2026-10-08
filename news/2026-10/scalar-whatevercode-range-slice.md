# Scalar indexed by a WhateverCode range slices a one-element list

`$scalar[0 .. *-2]` returned `Nil` for a non-list scalar; it now slices the
one-element list `($scalar,)` like Rakudo, so an empty range is `()` and
`.sprintf($line[0 .. *-2])` passes no arguments. Found via the Text::Diff
distribution, whose `t/general.t` and `t/table.t` now pass.
