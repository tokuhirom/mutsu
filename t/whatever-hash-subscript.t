use v6;
use Test;

# `*{"key"}` is a WhateverCode hash-subscript (curries to `*.{"key"}`), exactly
# like `*<key>` and `*[0]`. The `{...}` postcircumfix used to be attached only to
# a fixed set of base expressions (Var/BareWord/Index/...), omitting Whatever, so
# `*{$col}` left the `{...}` as a separate block and failed to parse as a call
# argument (e.g. `.classify(*{$col})` in ML::SparseMatrixRecommender).

plan 8;

# Bare WhateverCode hash-subscript, invoked directly.
{
    my %h = k => 42;
    my $f = *{"k"};
    is $f(%h), 42, '*{"key"} curries to a hash-subscript closure';
}
{
    my %h = a => 1, b => 2;
    my $f = *{"b"};
    is $f(%h), 2, '*{"key"} selects the right value';
}

# As a method-call argument (the failing shape).
{
    my @d = { n => "a", v => 1 }, { n => "b", v => 2 }, { n => "a", v => 3 };
    is @d.classify(*{"n"}).keys.sort, ("a", "b"), '*{key} as .classify argument';
    is @d.map(*{"v"}), (1, 2, 3), '*{key} as .map argument';
}

# Variable key.
{
    my $col = "name";
    my %r = name => "x", val => 9;
    my $f = *{$col};
    is $f(%r), "x", '*{$var} uses the runtime key';
}

# Parity with the already-working angle / positional Whatever subscripts.
{
    my %h = k => 7;
    is (*<k>)(%h), 7, '*<key> still works (angle)';
    my @a = 10, 20, 30;
    is (*[1])(@a), 20, '*[idx] still works (positional)';
}

# Regression: a real block after Whatever-bearing code is not a subscript.
{
    is (1, 2, 3).map(* + 1), (2, 3, 4), '* + 1 WhateverCode unaffected';
}
