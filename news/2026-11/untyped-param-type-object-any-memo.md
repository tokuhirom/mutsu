# An untyped parameter handed a type object checks `Any` from a memo

Every untyped routine parameter carries an implicit `Any` constraint. For an
instance of a user class that check was already answered per class from the
type-check memo (#12875), but a *type object* argument (`f(Int)`, Monad's
`some-type(Mu \M, ...)` callers) took the full resolve path on every call.

`instance_matches_core_any` now also memoizes a type object, keyed by the
type's storage name under its own `MemoCheck::CoreAnyTypeObject` tag and
dropped whenever the registry's write generation moves (#17376).

On a micro-benchmark of 20k calls handed `Any`, callgrind counts fell from 250.9M to
192.4M instructions (`f($x)` handed a type object, -23%) and from 238.3M to
179.6M (`f(\x)` handed a type object, -25%). `f(\x)` with a type object is now
1.79x `f($x)` with an `Int` (was 2.33x), which meets the issue's 2x
sub-goal.

While measuring this, #18804 turned up: an `Any` parameter admits the type
object of a `class M is Mu`, which raku rejects. That bug predates this change
and is filed separately.
