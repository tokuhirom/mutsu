# Slot method frames read attributes and divide

The slot method frame (ADR-18393) runs a planned method call without an env
overlay, by-name parameter copies or most of the lean frame's bookkeeping, but
it admitted only bodies whose every local read was a plain slot. A method that
reads an attribute (`method area { $!w * $!h }`) or divides
(`$!b * $!h / 2`) took the lean frame instead, about 4.6k instructions of
frame work per call (#19293).

A slot frame now reads a scalar attribute (`$!x`, `$.x`) straight from the
receiver's attribute cell, through the same attribute-site cache the lean
frame uses, and admits `Div`. The cell is the attribute's only storage, so a
write made by another method, an `rw` accessor, a user operator the body calls
or a bound container is always seen. Entry still falls back to the lean frame
when an atomic variable, a cross-thread dirty copy or a container request is
live, since the general `GetLocal` arm consults those before the cell, and a
CStruct owner keeps the lean frame. The frame no longer copies attributes into
slots it never reads.

`benchmarks/poly-call.raku` (one run, `BENCH_DET=1`) went from 248.8M to 218.7M
instructions (-12.1%) and from 41.1 ms to 39.2 ms wall (rakupp v5.9.0: 21.2 ms).
