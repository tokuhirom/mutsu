use Test;

# A qualified call `self.Role::method` to a parameterized role must resolve to
# the concretization the receiver consumed (binding its type parameter), even
# when a same-named *unparameterized* role also exists (the two collide in the
# by-name role registry).

plan 4;

{
    my sub ts(Mu \a, Mu $b = Nil) { a.^name ~ "/" ~ ($b ~~ Nil ?? "NIL" !! $b.^name) }
    my role R1[::T] { method of-type { ts($?ROLE, T) } }
    my role R1     { method of-type { ts($?ROLE) } }
    my class C1 does R1[Int] {
        method of-type { ("C1", self.R1::of-type) }
    }
    is-deeply C1.new.of-type, ("C1", "R1/Int"),
        'qualified call resolves to the consumed parameterized concretization (T bound)';
}

{
    # Different concretization over the same roles.
    my sub ts(Mu \a, Mu $b = Nil) { a.^name ~ "/" ~ ($b ~~ Nil ?? "NIL" !! $b.^name) }
    my role R1[::T] { method of-type { ts($?ROLE, T) } }
    my role R1     { method of-type { ts($?ROLE) } }
    my class C2 does R1[Str] {
        method of-type { ("C2", self.R1::of-type) }
    }
    is-deeply C2.new.of-type, ("C2", "R1/Str"),
        'qualified call binds a different consumed type parameter';
}

{
    # No unparameterized sibling: still works (regression guard for the common case).
    my role R1[::T] { method of-type { "T=" ~ T.^name } }
    my class C3 does R1[Int] {
        method of-type { "C3 | " ~ self.R1::of-type }
    }
    is C3.new.of-type, "C3 | T=Int",
        'single parameterized role + override + qualified call';
}

{
    # A method NOT overridden by the class is still reachable via the qualifier.
    my role R1[::T] { method who { "R1[" ~ T.^name ~ "]" } }
    my class C4 does R1[Int] { }
    is C4.new.who, "R1[Int]", 'unoverridden role method binds T';
}
