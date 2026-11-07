# `"a".."zz"` stops at `zz`

A string range expanded while the current string sorted at or before the end,
so `("a".."zz")` went on through three-letter strings (`zyz`, ...): 18252
elements instead of 702. The expansion now also stops once the successor is
longer than the end, as Rakudo's string ranges do.
