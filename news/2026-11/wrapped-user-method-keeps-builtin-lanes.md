# Wrapping a user method no longer takes every built-in call off the method lanes

The method-table site lane and the iterator site lane serve a built-in method call from its row.
They stepped aside whenever the program held *any* method `.wrap` chain (`has_any_wrap_chains()`).
That rule exists because a wrapped built-in method must run its wrapper, not the row (#12506).
The side effect: a program that wraps one user method lost both lanes for every built-in call.
Method::Protected's `is protected` wraps a user method, so its programs paid this on all of them.

Both lanes now ask `wrap_chains_may_reach(receiver)` instead. A chain owned by a user class cannot
reach a built-in receiver, because a built-in type never inherits from a user class. So the lanes
step aside only when some owner is a built-in (catalog) class, or the receiver is not a built-in
value. The owner set is `Registry::method_wrap_owners` (#18147).

Callgrind slope (profiling build, N 300 -> 900) in a program that wraps one user class's `pick`:

| | before | after |
| --- | ---: | ---: |
| `$s.uc; @a.elems; $s.substr(0, 2).lc` | 35.9k Ir | 20.3k Ir |
| `@words.pick` | 16.9k Ir | 4.3k Ir |
| Method::Protected `is protected` pick, `srand(42)` | 190.6k Ir | 178.6k Ir |

With no wrap at all, the same `@words.pick` costs 3.3k Ir.

`t/oo/method/wrap-method-builtin-row-lane.t` pins three cases: a wrapped user method still runs its
wrapper; built-in methods of the same names run the built-in; and a wrap of `Str.lc` itself still
runs through the lane's receivers until it is restored. Refs #12875.
