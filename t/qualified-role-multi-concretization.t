use Test;

# Qualified `self.Role::method` resolution when a class composes several
# parameterized roles, including role-to-role type-parameter forwarding
# (`role R2[::T] does R1[::T]`). A concretization the class declares DIRECTLY
# (`does R1[Int]`) must resolve to that direct concretization and bind its own
# type parameter — a same-base concretization reached only transitively (via
# `does R2[Num]`) must not make the direct one ambiguous, and the per-role type
# parameters must not collide. Distinct role names per case avoid the global
# role-registry name collision between independent lexical roles.

plan 4;

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
