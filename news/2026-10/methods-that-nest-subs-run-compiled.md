# A method that nests subs runs its compiled body as a callable

The callable `.^find_method` returns, and the value of a class-body `my method`,
ran through the tree-walk carrier whenever the method nested a `sub`: that was
the guard kept after `^add_method` dropped the nested-sub table (fixed in the
previous slice). With the table carried everywhere, the guard is gone, so a
monitor method or any method with an inner `sub` runs the chunk it was compiled
to. A slice of #14572.
