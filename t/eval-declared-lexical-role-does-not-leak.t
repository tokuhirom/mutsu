use v6;
use Test;

# A `my role` declared inside an EVAL'd string must not register itself in a
# way that shadows a later, lexically scoped role of the same short name.
# The EVAL'd role deliberately declares NO methods: if the leak came back,
# the lexical R1[Int] below would resolve to the method-less EVAL'd version
# and `.x` would die with "No such method". (An earlier repro that gave both
# roles the same method looked green even while the leak was present.)

plan 4;

my $eval-ok = True;
try {
    EVAL 'my role R1[::T] { }; my R1 of Str $x = R1[Int].new;';
    CATCH { default { $eval-ok = True } }
}
ok $eval-ok, 'EVAL declaring a parametric my role runs (its own type error is fine)';

{
    my role R1[::T] { method x { T } }
    my $r = R1[Int].new;
    ok $r.can('x'), 'lexical R1 after the EVAL still has its own method';
    is $r.x.^name, 'Int', 'lexical R1[Int].x resolves through the lexical role';
}

{
    # A second, unrelated lexical role with the same name in another scope
    # must also be independent of the EVAL'd one.
    my role R1[::T] { method y { T.^name ~ '!' } }
    is R1[Str].new.y, 'Str!', 'a further lexical R1 is independent too';
}
