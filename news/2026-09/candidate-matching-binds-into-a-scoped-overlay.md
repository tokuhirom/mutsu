# Multi candidate matching stopped deep-copying the frame env per candidate

`Interpreter::method_args_match_for_invocant` decides whether one `multi method`
candidate accepts a call. It is a *speculative* window: it binds the role's type
parameters, the candidate's own `role_param_bindings`, the method's captured
lexical scope, `self`, and any `::T` type captures; it runs the `where`
constraints; and then it rolls the whole lot back. A multi with four candidates
runs it four times per call.

It bound all of that into the running frame's own env. And the rollback needs a
snapshot -- `let saved_env = self.env.clone()` -- which shares the env's `Arc`.
So the *first* of those binds hit `Arc::make_mut` and deep-copied the entire map.
Every candidate of every multi method call paid one O(env) copy of a map whose
size is the number of names in scope.

It binds into an `Env::scoped_child` overlay now. That is exactly what scoped
envs are for, and their doc comment already said so: "an empty overlay that reads
through to `parent` ... so the inherited entries are never `make_mut`-deep-copied".
The same writes are O(1) into the empty tier, and the rollback is dropping the
tier rather than restoring a copied map. `restore_env_preserving_dynamics` gets
faster for free: the `iter()` it scans for dynamic-variable writes now walks the
overlay -- exactly the writes made in the window -- instead of every name in
scope.

## Measurement

`env_deep_copies` counted these, but the count could not distinguish a copy of a
900-entry frame env from a copy of an empty overlay, and only the first is a cost
that grows with the program. So the counter gained a companion,
`env_deep_copy_entries`, which sums the map's length at each copy. That is the
number that matters, and it is what the regression test asserts.

Measured as the slope against the call count, so the program's fixed setup cancels
out. A `class` with two `multi method` candidates, one `where`-guarded, called 50
and then 250 times:

| | entries deep-copied per call |
| --- | --- |
| before | 121 |
| after | 1 |

Independent of how many names are in scope, which was the point: padding the file
scope with 400 extra lexicals leaves the slope at 1.

On the HTTP/2 request parser this is the largest single source of env deep copies
(547 copies of an 888-entry env over a 20-frame run), and removing it takes a
HEADERS frame from 15.4 ms to 13.7 ms.

## Found while writing the tests

A `where` clause on a method parameter cannot read `$.attr` through the invocant
(`multi method f($x where { $x > $.limit })` never matches). That is a
long-standing gap, not a regression -- it fails identically before and after this
change -- and is filed as
[#7799](https://github.com/tokuhirom/mutsu/issues/7799).
