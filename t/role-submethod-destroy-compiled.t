use Test;
plan 3;

# §B: role-composed BUILD/TWEAK submethods (run via the construction MRO, which
# previously called run_instance_method_resolved directly) now go through the
# shared compiled-or-treewalk helper. (DESTROY submethods use the same helper;
# their behaviour is covered by the roast destruction suite.)
{
    role R1 { has $.tag is rw; submethod BUILD { self.tag = "rbuild" } }
    class C1 does R1 { }
    is C1.new.tag, "rbuild", 'role BUILD submethod runs (compiled helper)';
}
{
    role R2 { has $.n is rw = 10; submethod TWEAK { self.n = self.n + 7 } }
    class C2 does R2 { }
    is C2.new.n, 17, 'role TWEAK submethod runs after init (compiled helper)';
}
# Attribute set by a role BUILD is visible afterwards (attr-threading preserved).
{
    role R3 { has $.a is rw; has $.b is rw; submethod BUILD { self.a = 3; self.b = self.a * 2 } }
    class C3 does R3 { }
    my $o = C3.new;
    is "{$o.a},{$o.b}", "3,6", 'role BUILD attribute-threading preserved';
}
