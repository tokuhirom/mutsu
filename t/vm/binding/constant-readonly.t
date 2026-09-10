use Test;

plan 8;

# Sigilless constant
{
    constant foo = 42;
    is foo, 42, 'sigilless constant works';
    dies-ok { foo = 3 }, 'cannot reassign sigilless constant';
}

# Sigiled constant
{
    constant $bar = 42;
    is $bar, 42, 'sigiled constant works';
    dies-ok { $bar = 2 }, 'cannot reassign sigiled constant';
}

# Array constant
{
    constant @arr = 1, 2, 3;
    is @arr.elems, 3, 'array constant has correct elements';
}

# Hash constant
{
    constant %h = a => 1, b => 2;
    is %h<a>, 1, 'hash constant works';
}

# constant ($a, $b) = ... should be a parse error
{
    throws-like 'constant ($a, $b) = (3, 4)', X::Syntax::Missing,
        'constant list destructuring is a syntax error';
}

# Constant in try block should still be readonly
{
    constant grtz = 42;
    try { grtz = 23 };
    is grtz, 42, 'constant unchanged after failed try assignment';
}
