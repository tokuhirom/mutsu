# `Range.AT-POS` with a negative index answers an out-of-range Failure

`(1..3).AT-POS(-1)` answered `Nil`; like Rakudo (and like `List`/`Capture`) it is now an
`X::OutOfRange` Failure ("Index out of range. Is: -1, should be in 0..^Inf"), and
`(1..3).EXISTS-POS("a")` dies on the non-numeric `Str` instead of answering `False` (#12059).
