use Test;

# A parameterized QuantHash (`BagHash[Int]`, `MixHash[Int]`, `SetHash[Str]`, ...)
# constrains its *keys*. Assigning an element under a key of the wrong type must
# throw X::TypeCheck::Binding. Numeric-string keys (`<7>`) are allomorphs and
# satisfy a numeric key type.

plan 9;

# BagHash[Int]: non-numeric Str key rejected, numeric keys accepted.
{
    my %bh is BagHash[Int] = 1, 2, 3;
    is %bh.keyof, Int, 'BagHash[Int] keyof is Int';
    throws-like { %bh<foo> = 42 }, X::TypeCheck::Binding,
      'BagHash[Int]: assigning a non-numeric Str key fails';
    lives-ok { %bh<7> = 3 }, 'BagHash[Int]: numeric-string key <7> is allowed';
    is %bh<7>, 3, 'BagHash[Int]: <7> weight set';
    lives-ok { %bh{8} = 2 }, 'BagHash[Int]: Int key 8 is allowed';
}

# MixHash[Int]: same key constraint.
{
    my %mh is MixHash[Int] = 1, 2, 3;
    throws-like { %mh<foo> = 42e0 }, X::TypeCheck::Binding,
      'MixHash[Int]: assigning a non-numeric Str key fails';
}

# SetHash[Str]: string keys are fine.
{
    my %sh is SetHash[Str];
    lives-ok { %sh<abc> = True }, 'SetHash[Str]: Str key is allowed';
    is %sh<abc>, True, 'SetHash[Str]: element set';
}

# Unparameterized BagHash accepts any key type (no constraint).
{
    my %b is BagHash;
    lives-ok { %b<foo> = 5 }, 'plain BagHash accepts a Str key';
}
