# A finite pipe's list coercion is reified through a variable too

A list coercion of a finite `.map`/`.grep` pipe over a `gather` used to answer wrongly when the
receiver was a variable:

| expression | mutsu answered | now |
|---|---|---|
| `my $x = gather { take 1; take 2 }.grep(* > 0); $x.List` | `()` | `(1 2)` |
| the same `$x.Array` | `[(1 2)]` | `[1 2]` |
| the same `$x.list.elems` | `1` | `2` |
| the same `$x.values` | `((2 3))` (also inline) | `(2 3)` |

The inline form was already right. The VM compiles a call on an expression and a call on a
variable to different opcodes, and each kept its own copy of when to reify a lazy receiver. The
copies had drifted apart: the variable lane never reified a laziness-preserving coercion of any
pipe.

Both lanes now read one decision (`lazy_lane_reifies`). A pipe that must stay lazy (infinite, or
marked `lazy`) still does. The `values` pull now answers only a lazy receiver; a finite one is
reified like any other method (ADR-13891, #11276).
