use Test;

plan 14;

# Basic scalar default
{
    my $x is default(42);
    is $x, 42, 'uninitialized scalar with is default returns the default';
}

# Scalar with explicit initialization
{
    my $x is default(42) = 10;
    is $x, 10, 'scalar with is default and explicit init has the init value';
}

# Assigning Nil reverts to default
{
    my $x is default(42) = 10;
    $x = Nil;
    is $x, 42, 'assigning Nil to scalar with is default reverts to default';
}

# Default with string value
{
    my $x is default("hello");
    is $x, "hello", 'string default works';
    $x = "world";
    is $x, "world", 'assignment overrides string default';
    $x = Nil;
    is $x, "hello", 'Nil reverts to string default';
}

# Array default
{
    my @a is default(0);
    is @a[5], 0, 'array with is default returns default for missing index';
}

# Hash default
{
    my %h is default("none");
    is %h<x>, "none", 'hash with is default returns default for missing key';
}

# Default with expression
{
    my $x is default(21 + 21);
    is $x, 42, 'default with expression works';
}

# Array with explicit elements and default
{
    my @a is default(0);
    @a[0] = 5;
    is @a[0], 5, 'array element set explicitly retains value';
    is @a[1], 0, 'array element not set returns default';
}

# Multiple Nil assignments
{
    my $x is default(99);
    $x = 1;
    $x = Nil;
    is $x, 99, 'second Nil assignment still reverts to default';
}

# Typed scalar defaults with smileys
{
    my Int:D $x is default(0);
    is $x, 0, 'Int:D scalar accepts a concrete default value';
    throws-like { EVAL 'my Int:U $y is default(0);' }, X::Parameter::Default::TypeCheck,
        'Int:U scalar still rejects a concrete default value';
}
