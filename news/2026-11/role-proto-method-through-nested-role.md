# A role's `proto method` body survives composition through another role

`role R { proto method render(...) { my $r = {*}; wrap($r) } ... }` composed directly into a class
ran its proto body, but composed through another role (`role S does R; class C does S`) the proto
was never installed on the class, so a call went straight to the multi candidates and the wrapper
body (SQL::Abstract's `parenthesize-if(...)` around a rendered `Row`, so `IN ($1, $2, $3)` lost its
parentheses) never ran.

The ancestor-role composition path now installs the proto like the direct one does. The installed
proto's package is the role that declared it (it was the composing class), so the body also sees the
role's own `sub` helpers; a direct composition of such a role died with `Unknown function` before.

Found by SQL::Abstract (`t/01-basic.rakutest` tests 17, 23, 24).
Test: `t/oo/role/role-proto-method-through-nested-role.t`.
