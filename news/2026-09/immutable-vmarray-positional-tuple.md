# An immutable VMArray positional stays immutable: Tuple passes

The `Tuple` distribution (`class Tuple is repr('VMArray') is ValueList`) now
passes its whole suite, 27/27 (#10261). `ValueList` makes itself immutable by
installing throwing `ASSIGN-POS`/`BIND-POS`/`push`/... methods with
`^add_method`; three paths still wrote into it, and fixing them surfaced two
more general bugs.

**`@a[0] := 42` ignored the class's own `BIND-POS`.** The subscript-bind site
found the override and called it, but `call_method_with_values` answered
`BIND-POS`, `push`, `append`, `List` and the rest from IterationBuffer's native
arms before consulting the class. Those arms now yield to a subclass method of
the same name.

**Iterating one aliased writable slots.** A VMArray object's slots hold the
stored objects themselves, not Scalar containers, so `$_ = 42 for @tuple`,
`for @tuple -> \v { v = 42 }` and `for @tuple.kv -> \k, \v { v = 42 }` all die
in rakudo. mutsu let the assignment through and dropped it. A `for` whose
source is such an object now marks the topic or the parameter read-only for
each item that is not a container, and a multi-parameter loop reports the
write as `X::Assignment::RO`, the same way an immutable QuantHash source does.

**`Any.kv`/`.pairs`/`.antipairs`/`.keys`/`.values` ignored a `list` override.**
Rakudo defines them as `self.list.<method>`, so a class overriding `list`
answers from that list. mutsu treated the object as one item, so `Tuple.kv`
was `(0, Tuple.new(...))`. The native method dispatch now takes the class's
`list` first.

**An `is raw`/`is rw` routine lost a List or object tail.** `sub f is raw {
my $v = (1, 2); $v }` returned `Any`. The tail compiles to a container
capture, which does not re-box a local holding a reference value, and the
capture op mistook that for "no local slot here" and answered from a stale env
entry. ValueList's `!SET-SELF` (`my $valuelist = self` as an `is raw` tail) is
where it showed up.
