# Pin: a block's `use` through a run-time `use lib` path stays scoped

A block's `use` of a module found only through a computed `use lib` path takes
the run-time import path. The exported type is visible in the block only: not
to a routine written outside it, and not to a later EVAL. This already matches
Rakudo on main (the #14140 slices and #14235 closed it); the new test pins it
(#14202).
