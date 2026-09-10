use Test;

plan 7;

# `.=` with a string-interpolated method name whose `{ … }` block is a full
# statement list (not just one expression).
{
    my $c = 0;
    (my Int $x .= new).="{$c++; "new"}"(42);
    is-deeply $x, 42, '.= on an inline-declared var with a string-block method name';
    is-deeply $c, 1, 'method-name block ran exactly once';
}

# Same, but the outer `.=` target is a `do { … }` block whose value is an lvalue.
{
    my $c = 0;
    my $b = 0;
    my Int $x;
    do { $b++; ($x .= new) }.="{$c++; "new"}"(42);
    is-deeply $x, 42, '.= through a do-block lvalue target';
    is-deeply "$b $c", '1 1', 'do-block and method-name block each ran once';
}

# A `$`-scalar with a parameterized container type matches the WHOLE value, not
# its elements (element matching is only for `@`-sigil typed arrays).
{
    my Array[Numeric] $x = Array[Numeric].new(1, 2, 3);
    is-deeply $x, Array[Numeric].new(1, 2, 3), 'scalar parameterized-array type matches whole value';

    my Array[Numeric] constant foo .= new: 1, 2, 3;
    is-deeply foo, Array[Numeric].new(1, 2, 3), 'parameterized-array constant via .=new';
}

# Regression guard: `@`-sigil element typing still rejects bad elements.
{
    dies-ok { my Int @a = 1, "x", 3 }, 'typed array still element-checks';
}
