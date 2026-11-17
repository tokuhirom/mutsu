# A method on a plain role's type object runs on one persistent pun

`role Q { method m {...} }; Q.m` composed a class named after the role on
every call, ran the method on it, and withdrew the class again. The withdrawal
was needed because a class of the role's own name left in the registry changed
later role composition and construction. Each call rebuilt the pun, cloning
every method definition, and wrote the registry twice. That also moved the
write generation that every generation-keyed cache is dropped by.

Rakudo keeps one pun per role. mutsu now does the same: the pun lives under
the storage name `Q\0pun` (the `\0` suffix is the mangling `.^name` already
strips, as for a parametric role's `P\0default[Any]`). It is composed once,
through the ordinary `class ... does Q` declaration path. A method call on
the role type object is redirected to it through the same per-`(role, method)`
memo the default-parameterized roles use.

That also made the method's view of itself match rakudo's: `self.HOW` is a
`ClassHOW` (it was the role's `ParametricRoleGroupHOW`), and the role body runs
once instead of once per call.

Release build, a 5000-call loop: `Q.m` 78.6 µs → 3.3 µs per call, against
2.4–3.0 µs for the same method on a class (rakudo: 0.6 µs). Closes #17408.
