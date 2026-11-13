# A method call on a parameterized role goes straight to its pun

`R[Int].method` (and `$M.some(...)` with `$M = R.^parameterize(T)`) is
answered by the role's pun class. mutsu already remembered which pun such a
call re-dispatches to, but it consulted that memo deep inside the general
`call_method_with_values` path: the call first walked the `CallMethod` site's
whole probe chain, then the general path's probes, and only then re-dispatched
to the pun through the general path again.

The `CallMethod`/`CallMethodMut` site now swaps a parameterized role receiver
for its memoized pun before dispatching, which is the same re-dispatch the memo
hit performed. A trivial method on `R[Int]` went from ~46k to ~17k
instructions (callgrind slope); Monad's `t/14_concurrent_types.t` chain
iteration from 6.70M to 6.52M (#17331).
