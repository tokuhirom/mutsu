use Test;

plan 6;

# Qualified method dispatch through roles applied at run time via `does`,
# interleaved with qualified calls to compose-time roles and parent classes.
{
    my role Foo1 { method foo { "Foo1::foo" } }
    my role Foo2 { method foo { "Foo2::foo" } }
    my role Foo3 { method foo { "Foo3::foo" } }

    my class Foo { method foo { "Foo::foo" } }

    my class Bar is Foo does Foo1 {
        method run {
            my @res;
            @res.push: "Bar::foo";
            @res.push: self.Foo::foo;
            @res.push: self.Foo1::foo;

            self does Foo2;
            @res.push: self.Foo::foo;
            @res.push: self.Foo1::foo;
            @res.push: self.Foo2::foo;

            self does Foo3;
            @res.push: self.Foo::foo;
            @res.push: self.Foo1::foo;
            @res.push: self.Foo2::foo;
            @res.push: self.Foo3::foo;
            @res
        }
    }

    is-deeply Bar.new.run,
        [<Bar::foo Foo::foo Foo1::foo Foo::foo Foo1::foo Foo2::foo Foo::foo Foo1::foo Foo2::foo Foo3::foo>],
        "qualified dispatch survives interleaved run-time role mixin";
}

# A run-time mixin must persist across a qualified parent-class call.
{
    my role R { method tag { "R::tag" } }
    my class P { method tag { "P::tag" } }
    my class C is P {
        method check {
            self does R;
            my $parent = self.P::tag;   # qualified parent call
            return ($parent, self.R::tag, self.does(R));
        }
    }
    my ($p, $r, $d) = C.new.check;
    is $p, "P::tag", "qualified parent call returns parent method";
    is $r, "R::tag", "run-time role still reachable after parent call";
    ok $d, "object still does the run-time role after a qualified parent call";
}

# Ambiguous concretization of a parametric role composed twice in one class.
{
    my role R1[::T] { method of-type { "R1" } }
    my class C1 does R1[Int] does R1[Str] {
        method of-type { self.R1::of-type }
    }
    throws-like { C1.new.of-type }, X::AdHoc,
        "ambiguous concretization in a class throws",
        message => 'Ambiguous concretization lookup for R1';
}

# Ambiguity arising inside a composed role (resolved against immediate roles).
{
    my role R1[::T] { method of-type { "R1" } }
    my role R2 does R1[Int] does R1[Bool] {
        method of-type { self.R1::of-type }
    }
    my class C2 does R2 { }
    throws-like { C2.new.of-type }, X::AdHoc,
        "ambiguous concretization in a role throws",
        message => 'Ambiguous concretization lookup for R1';
}
