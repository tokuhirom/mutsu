use Test;

plan 5;

# `has $.x does Role` mixes the role into the attribute, so reading the
# attribute returns a value that does the role and `$o.x.method` dispatches into
# it. Mirrors roast/S03-binding/attributes.t subtests 13-16.
{
    my role Foo { method bar { 42 } }
    my class A {
        has $.scalar does Foo;
        has @.array  does Foo;
        has %.hash   does Foo;
        has &.code   does Foo;
    }
    my $a = A.new;
    is $a.scalar.bar, 42, 'scalar attribute is mixed in';
    is $a.array.bar,  42, 'array attribute is mixed in';
    is $a.hash.bar,   42, 'hash attribute is mixed in';
    is $a.code.bar,   42, 'code attribute is mixed in';
}

# Multiple roles on one attribute.
{
    my role R1 { method m1 { 1 } }
    my role R2 { method m2 { 2 } }
    my class C { has $.x does R1 does R2 }
    my $c = C.new;
    ok $c.x.m1 == 1 && $c.x.m2 == 2, 'multiple does-roles on one attribute';
}
