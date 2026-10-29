# A chained comparison evaluates its first operand first

`(@a[0] = 0) == @a[0] == 0` read the shared middle operand into its temporary
before the first operand ran, so an assignment in the first operand was not
seen by the middle one. A first operand that is not a plain variable or
literal is now bound to a temporary ahead of the middle operand, restoring
left-to-right evaluation. This was one of the blockers for the `annotations`
distribution's `t/01-containers` Buffer subtest.
