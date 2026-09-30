# Array.grab and Mu.set_why on plain instances

`@a.grab`, `@a.grab($n)` and `@a.grab(*)` now remove and return random elements of an
`Array` (including `@!attr` arrays), and `$obj.set_why($pod)` on any non-HOW value attaches
the pod to its type, so `.WHY` reads it back. Found by the `Random::Names` ecosystem suite,
which now passes all 91 baseline assertions.
