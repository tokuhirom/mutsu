use Test;

# Qualified `self.Role::method` resolution when a class composes several
# parameterized roles, including role-to-role type-parameter forwarding
# (`role R2[::T] does R1[::T]`). A concretization the class declares DIRECTLY
# (`does R1[Int]`) must resolve to that direct concretization and bind its own
# type parameter — a same-base concretization reached only transitively (via
# `does R2[Num]`) must not make the direct one ambiguous, and the per-role type
# parameters must not collide. Distinct role names per case avoid the global
# role-registry name collision between independent lexical roles.

plan 7;

my sub ts(Mu \a, Mu $b = Nil) { a.^name ~ ($b ~~ Nil ?? "" !! "[" ~ $b.^name ~ "]") }

{
    my role P1[::T] { method of-type { ts($?ROLE, T) } }
    my role P2[::T] does P1[::T] { method p2 { "P2" } }
    my class CP does P1[Int] does P2[Num] {
        method via-p1 { self.P1::of-type }
    }
    is CP.new.via-p1, "P1[Int]",
        'directly-declared concretization wins over a transitive same-base one';
}

{
    my role Q1[::T] { method t-name { T.^name } }
    my role Q2[::T] does Q1[::T] { }
    my class CQ does Q2[Str] { }
    is CQ.new.t-name, "Str", 'role-to-role ::T forwarding composes the parent role';
}

{
    my role A1[::T] { method a-type { ts($?ROLE, T) } }
    my role B1[::T] { method b-type { ts($?ROLE, T) } }
    my class CAB does A1[Int] does B1[Str] {
        method both { (self.A1::a-type, self.B1::b-type) }
    }
    is-deeply CAB.new.both, ("A1[Int]", "B1[Str]"),
        'distinct roles bind distinct type parameters without collision';
}

{
    my role S1[::T] { method of-type { ts($?ROLE, T) } }
    my role S2[::T] does S1[::T] { }
    my class CS does S1[Str] does S2[Int] {
        method via-s1 { self.S1::of-type }
    }
    is CS.new.via-s1, "S1[Str]", 'direct concretization resolves with its own type';
}

{
    # Context-relative resolution: `self.W1::of-type` called from INSIDE a
    # forwarding role's method must see that role's forwarded concretization
    # (W2[Num] forwards W1[Num]), not the receiver class's own direct W1[Int].
    my role W1[::T] { method of-type { ts($?ROLE, T) } }
    my role W2[::T] does W1[::T] { method from-r2 { self.W1::of-type } }
    my class CW does W1[Int] does W2[Num] {
        method outer { (self.W1::of-type, self.W2::from-r2) }
    }
    is-deeply CW.new.outer, ("W1[Int]", "W1[Num]"),
        'qualified call resolves relative to the executing role context';
}

{
    # Inherited method resolves relative to its DEFINING class, not the receiver.
    # `IC.via` is inherited from `IP`; `self.IR::of-type` inside it must see IP's
    # `IR[Int]` (IP's role), even though IC itself also does `IR[Str]`.
    my role IR[::T] { method of-type { ts($?ROLE, T) } }
    my class IP does IR[Int] { method via { self.IR::of-type } }
    my class IC is IP does IR[Str] { }
    is IC.new.via, "IR[Int]",
        'inherited method resolves its qualified role call against its defining class';
}

{
    # Full multi-role + forwarding stanza (the qualified.t subtest-6 shape):
    # C does R1[Int] does R2[Num], R2[::T] does R1[::T]. `self.R1::of-type` from C
    # sees R1[Int]; from within R2[Num]'s method sees R1[Num].
    my role G1[::T] { method of-type { ts($?ROLE, T) } }
    my role G2[::T] does G1[::T] { method of-type { (ts($?ROLE, T), self.G1::of-type) } }
    my class GC does G1[Int] does G2[Num] {
        method of-type { (self.G1::of-type, |self.G2::of-type) }
    }
    is-deeply GC.new.of-type, ("G1[Int]", "G2[Num]", "G1[Num]"),
        'multi-role + forwarding: direct vs forwarded concretizations resolve correctly';
}
