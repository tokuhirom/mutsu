# A stored gather's list view stays lazy

Calling `.list` on a gather held in a variable now preserves its live iterator. A `for` loop consumes taken elements as they arrive, so elements taken before a later exception still reach the loop body. The same applies to the `<>` list view. Reifying a stored `.List` also preserves scalar itemization, so it renders as `$(...)`.
