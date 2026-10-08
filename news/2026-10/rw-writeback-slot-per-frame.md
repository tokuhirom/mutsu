# rw writeback slots are keyed per call frame

`pending_rw_writeback_slots` mapped a writeback source name to one baked caller
slot, so a nested call forwarding a same-named variable (`sub mk(\blob, $offset
is rw) { Cur.new(blob, $offset) }` called as `mk($b, $offset)`) replaced the outer
frame's pending entry. The outer drain then retained the source for a frame that
no longer matched, and the caller's slot kept a plain value while its env held the
cell, tripping the ADR-0097 section 15 env/slot invariant (#11927). The table is
now keyed by `(name, owner depth)`, so each frame's entry survives nested calls.
