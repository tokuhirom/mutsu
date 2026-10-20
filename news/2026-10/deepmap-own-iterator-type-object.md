# `flatmap`, `deepmap` and `duckmap` read a receiver's own iterator

A class that declares only `iterator` answers `Any`'s `map`, `grep`, `sort`
and the rest from its elements (ADR-11276 §9.148), but `flatmap`, `deepmap`
and `duckmap` treated the object itself as one item: `PI.new.flatmap({ ($_,
$_) })` answered `(PI.new PI.new)` where Rakudo answers `(4 4 5 5)`. The three
names join the measured set of methods that read `self.iterator`.

A type object is now one item to `deepmap` and `duckmap`, as in Rakudo: the
block is called on it, and the result is wrapped in an `Array` for `Array` and
its subclasses (`Array.deepmap(&op)` is `[op(Array)]`) and in a `List`
otherwise (`Int.deepmap(&op)` is `(op(Int),)`). Before, the type object was
returned unchanged, at the top level and nested (`[Int, 1].deepmap(...)`)
alike (#13690).
