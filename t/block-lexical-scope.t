use Test;

plan 3;

is(
    (try { my $x = 1; { $x = 2; }; $x }),
    2,
    'assignment to outer lexical inside block persists',
);

throws-like(
    '
    {
        my $inner = 42;
    }
    $inner;
    ',
    X::Undeclared,
    'block lexical does not leak',
);

throws-like(
    '
    {
        our $sa2 = my $sb2 = 42;
    }
    ($sa2, $sb2);
    ',
    X::Undeclared,
    "chained our/my in block does not leak",
);
