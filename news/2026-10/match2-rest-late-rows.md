# elems, Setty/Baggy/Mixy and a Supply's list are rows

`elems`, the quant hashes' family coercions (`SetHash.Baggy`, `Set.Mixy`, `Bag.Setty`) and a `Supply`'s `.list` and
`.Array` lost their name-matching arms in the interpreter's method cascade (ADR-11276 §9.136). `Supply.list`'s row now
does all three things the arm did:
- answers a materialized supply;
- drains a live, channel-backed one (an `IO::Socket::Async` read stream);
- collects an on-demand one.
