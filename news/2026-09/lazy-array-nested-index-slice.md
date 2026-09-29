# Lazy-array slices with nested index lists reify far enough

`@lazy[^2, 20, 30, 40]` returned `Nil` for the last index because the up-front
reification bound only understood flat integer index lists. The bound is now
computed recursively through nested lists and finite ranges.
