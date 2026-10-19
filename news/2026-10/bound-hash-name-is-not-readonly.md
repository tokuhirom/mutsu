# A `:=`-bound hash name is no longer marked readonly

`my %a := %b` used to mark `%a` readonly as a "bind signal". It then planted
a second env key, `__mutsu_bound::%a`, so that every readonly check could
undo that mark again for a bound hash, while a `constant %M` stayed immutable.
Three readers had to agree on the pair: `CheckReadOnly`, the by-name
`SetGlobal` path and the element store.

Under ADR-0097, a binding's own facts should not live under keys derived from
its name, and this one did not need to exist at all. The bind now leaves the
name writable, and the value decides instead. A `%` variable that holds a
`Map`, `Set`, `Bag` or `Mix` got it from a bind or an `is Map` declaration,
because assignment coerces to a `Hash`. A whole assignment to such a variable
raises `X::Assignment::RO` from the store, through the slot path and the
closure path alike (`immutable_associative_assignment`).

`Stmt::MarkBoundContainer`, `MetaNs::Bound` and its process-wide latch are
gone. Fixed along the way: a whole assignment to a hash bound to a `Map`
(`my %m := Map.new(...); %m = ...`) used to succeed silently. Rakudo refuses
it, and mutsu now does too.
