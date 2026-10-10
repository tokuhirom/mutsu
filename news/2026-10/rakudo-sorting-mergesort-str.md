# Rakudo::Sorting.MERGESORT-str

`Rakudo::Sorting.MERGESORT-str` now sorts a native `str` array (an `nqp::list_s`) by codepoint and
returns the sorted native array, as Rakudo's does. `ValueMap.WHICH` (used by the `immutable`
ecosystem distribution) calls it on its keys. The comparison is the shared `str_order` routine.
