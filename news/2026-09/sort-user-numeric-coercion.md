`.sort({ $^a <=> $^b })` now dispatches user-defined numeric coercion
methods on object elements instead of comparing their object representations.

Closes #8586.
