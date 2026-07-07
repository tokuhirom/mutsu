use Test;

plan 5;

# A BEGIN inside a nested (bare) block runs at compile time, so its side
# effects are visible to reads (via a closure) that textually precede it.
{
    is baz(), 3, 'nested BEGIN init visible to earlier read (1)';
    is baz(), 4, 'nested BEGIN init visible to earlier read (2)';
    is baz(), 5, 'nested BEGIN init visible to earlier read (3)';

    my $a; BEGIN { $a = 3 };
    sub baz { $a++ }
}

# A declaration that precedes a BEGIN it depends on must stay before it: the
# BEGIN must not be hoisted above the class it references.
{
    class SomeClass { };
    my $var;
    BEGIN { $var = SomeClass };
    isa-ok $var, SomeClass, 'declaration before BEGIN is kept before it';
}

# A runtime initializer before a BEGIN is a barrier: the BEGIN stays in place
# so a later constant sees the BEGIN-updated value.
{
    my $foo = 42;
    BEGIN { $foo = 23 }
    constant timecheck = $foo;
    is timecheck, 23, 'constant initializer sees BEGIN-updated value';
}

# vim: expandtab shiftwidth=4
