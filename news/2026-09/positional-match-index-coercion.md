# Numify Match objects used as positional indices

Positional subscripts now call `.Int` on object indices, including native Cool
objects such as `Match`. A regex Match of `"1"` therefore selects element 1
from an Array, List, or Seq.
