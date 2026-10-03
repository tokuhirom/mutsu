# Every store into a `Scalar` holder now itemizes, and a share holder's reassignment no longer clobbers its source

ADR-0079's last open slice was an audit of the remaining producers of a `$`-holder container.
Instead of walking ~140 `Value::container_ref` call sites one by one, the audit probed every way an
aggregate can reach a `$` variable or an element and compared each result with rakudo.

The worst finding was a data-corruption bug in the `=`-share path. After `my $x = %h`, `$x` and `%h`
share one cell. A list assignment `($x, $y) = 7, 8` or an expression-position `($x = 5)` correctly
replaced `$x`'s slot, but the by-name environment write that followed still saw the shared cell and
stored `7` through it, so `%h` became `7`. The statement form `$x = 7` did not clobber the source,
but it left the environment naming the cell after the slot had moved on, which tripped the ADR-0097
§15 debug assertion on the very next read of `$x`. Both forms now detach the environment entry
together with the slot.

The other fixes are itemization. Each of these now reads back as `${...}` / `$[...]`, as in rakudo:

- `state $x = %h`;
- a chained `my $y = $x`, which no longer demotes `$x`'s own word to plain;
- a multi-dimensional element (`@a[0;1] = %h`, plain or shaped);
- `atomic-assign` and every form of `cas`.

The opposite case is `my \x = %h`. A sigilless binding is not a `Scalar`, so `x` now reads `{...}`
and flattens in a hash initializer.

Three problems turned up that need their own design work. They are filed as #11227 (a share holder
owns no `Scalar` of its own, so a write through a closure, an `is rw` parameter or a topic alias
reaches the source), #11228 (the compile-time set of sigilless names leaks into a later `my $x` with
the same name) and #11229 (`$_ = AGGREGATE` is never itemized). #11230 is an unrelated failure of
list assignment to an `is rw` accessor that the probes also hit.
