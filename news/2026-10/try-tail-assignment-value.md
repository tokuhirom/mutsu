# A `try` block's trailing assignment yields its value

`try { $x = 9 }` evaluated to Nil (or Any when assigned onward) because the
`try` region compiled a trailing `Stmt::Assign` as a plain statement. It now
uses the same tail-value rule as a `do` block, so the block yields the assigned
value (#11188).
