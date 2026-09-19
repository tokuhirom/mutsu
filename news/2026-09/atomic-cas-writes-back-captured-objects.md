Atomic compare-and-swap callbacks now write back captured object values, not
only scalar lexicals. This fixes object state updates in concurrent data
structures such as `Concurrent::Stack`.
