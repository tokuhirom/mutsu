# `.^find_method` finds methods declared on any built-in type

`DateTime.^find_method('timezone')` answered `(Mu)` because only a fixed set of built-in owners had
introspection rows, although `.^can` already consulted the native method row table. `find_method`
and `lookup` now fall back to the same declared rows along the MRO, so DateTime::Timezones loads and
its `t/02-speed` and `t/03-math` pass. `t/01-upgrade` still needs `.wrap` on built-in methods (#11314).
