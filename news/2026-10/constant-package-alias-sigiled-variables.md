# `$E::v` resolves through a `constant` that names a package

A `constant` bound to a package type object already worked as a qualifier for
calls (`E::f()`), code references (`&E::f`) and type barewords. A sigiled package variable was the one form that ignored it:

```raku
class A::B { our $v = 3 }
constant E = A::B;
say $E::v;   # was (Any), now 3
```

The variable read paths (`GetGlobal`, `GetArrayVar`, `GetHashVar` and the
`++`/`--` read) now fall back to the aliased name, `$A::B::v`, as their last
store. The scalar write chokepoint and the mutating-method root
(`env_root_descended_mut`) resolve it as well, so `$E::v = 5`,
`$E::v++`, `$E::v += 1` and `@E::a.push(...)` update the real variable (#11315).

Element and whole-container assignment through the alias (`@E::a[0] = 1`,
`@E::a = ...`) take other paths and are tracked in #11566.
