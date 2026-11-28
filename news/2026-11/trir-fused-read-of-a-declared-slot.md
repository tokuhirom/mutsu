# TRIR's fused list and string reads cover a slot declared from any initializer

`nqp::atpos_i($c, $i)` and `nqp::ordat($s, $i)` read their first operand straight out of a TRIR
slot (`AtPosILocal` / `OrdAtLocal`) when the slot is never reassigned, which avoids a clone and
drop of the boxed value per read. The declaration's own store counted as a reassignment unless
its initializer was an `nqp::` op, so `my $codes := text.NFD` in JSON::Fast's `str-escape` read
through the generic op on every character.

A declaration's store is now the slot's initialization, not a write. Because the fused form
reads the slot after the index operand runs, an index that rebinds the operand
(`nqp::atpos_i($c, ($c := $d, 0))`) declines the routine.

`str-escape` over long strings: 579 -> 454 instructions per character (callgrind slope, release).
`bench-json-fast-encode` (`scripts/bench-det.sh`, `+jit`, local A/B): 783.6M -> 771.3M
instructions (-1.6%).
