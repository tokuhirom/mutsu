# A role body does not inherit an enclosing `use fatal`

An outer `use fatal` no longer makes the methods of a role (parametric or not) fatal; a `use fatal` written inside the role body still does, from where it stands. Class methods under `use fatal` are unchanged. Measured against rakudo 2026.09 (#11452).
