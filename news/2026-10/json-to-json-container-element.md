# `to-json` serializes a hash element that was mutated in place as the value it holds

`%h<k>.push(1); %h<k>.push(2)` leaves the element boxed in a container cell, and the native JSON
encoder fell through to its "quoted stringification" fallback for a cell, so
`Rakudo::Internals::JSON.to-json(%h)` printed `{"k":"1 2"}`. The cell is read through now.
Pakku::Recman::Ecosystem writes its derived index buckets this way, so every ecosystem lookup came
back empty; its `recman` suite passes (mutsu#14870).
