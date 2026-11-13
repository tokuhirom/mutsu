# A `Mu` type check is answered before the instance memo

`Mu` accepts every value, but a `Mu`-typed parameter given an instance of a user class
(`method attr-var(Mu $instance is raw)`, `sub get-attr-obj(Mu \obj)`) still went through the
per-class instance memo first. Building that memo's key asks whether a sigilless constant or term
named `Mu` shadows the type, and on a miss that scope lookup walks the running package chain, the
module-scope tables and the `our` store -- about 20,000 instructions per bound parameter.

`type_matches_value_why` now answers a plain `Mu` constraint at once unless the program declared
its own class named `Mu`. AttrX::Mooish passes an instance through such parameters on every
lazy-attribute access: a method call with one `Mu $x` parameter given an instance drops from
about 10 us to 6 us, and one round of its `t/090-concurrent` workload (`new`, three predicates,
two lazy builds, a clear) from 6.37M to 5.92M instructions (-7%).

`t/types/mu-constraint-accepts-everything.t` pins what `Mu` accepts (an instance of a class that is
not `Any`, a type object, `Nil`, a Junction, a Proxy's value) and that `Any` still rejects a
non-`Any` instance.
