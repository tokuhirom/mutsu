# MY:: multi-key angle slices and Nil on a missing symbol

`MY::<$x $y>` (and `OUR::<...>` etc.) now parses as a slice of the listed names instead of one
key spelled "$x $y", so `MY::<&plan &pass>:p` returns the pairs. A symbol absent from a
`PseudoStash` (`MY::<&nope>`) now reads as `Nil`, as in Rakudo, rather than the `Any` type object.
Found with the `from` distribution; its remaining scope-locality gap is tracked in the issue linked
from the PR.
