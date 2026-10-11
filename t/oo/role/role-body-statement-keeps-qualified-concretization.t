use Test;

plan 2;

# A bare run-time statement in a parametric role's body must not change which
# concretization `self.Role::method` resolves to (#10679). Both spellings take
# the same route through role composition.
my sub ts(Mu \a, Mu $b = Nil) { a.^name ~ ($b ~~ Nil ?? "" !! "[" ~ $b.^name ~ "]") }

{
    my role P1[::T] { method of-type { ts($?ROLE, T) } }
    my role P2[::T] does P1[::T] { method p2 { "P2" } }
    my class CP does P1[Int] does P2[Num] {
        method via-p1 { self.P1::of-type }
    }
    is CP.new.via-p1, 'P1[Int]', 'method-only body';
}

{
    my $ran = 0;
    my role Q1[::T] { method of-type { ts($?ROLE, T) } }
    my role Q2[::T] does Q1[::T] { $ran++; method p2 { "P2" } }
    my class CQ does Q1[Int] does Q2[Num] {
        method via-q1 { self.Q1::of-type }
    }
    is CQ.new.via-q1, 'Q1[Int]', 'a run-time statement in the body changes nothing';
}
