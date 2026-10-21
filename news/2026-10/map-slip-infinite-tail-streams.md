# A `.map` callback that slips an infinite list streams it

`(1, 2).map({ slip(inf) }).head(3)` now answers `(0 1 2)`, as Rakudo does. Before, the
map output kept the slipped lazy list as one element (`((...) (...))`), because every map
loop collected each callback result into a `Vec` at once and had nowhere to keep a slipped
list it could not finish reading.

The loops still push into a `Vec`: a lazy-tail Slip that cannot be seen to end is pushed
whole, and the chunk settles it (`settle_map_slip_markers`). A deferred `.map` Seq keeps the
slipped list in its plan slot and reads it before the callback runs again; a lazy map pipe
and the multi-parameter map adaptor keep it in their own pending slot. An eager consumer
reads the list to its end, as it does any list.
