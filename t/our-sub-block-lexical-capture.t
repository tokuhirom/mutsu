use Test;

# An `our sub` declared inside a bare block closes over the block's `my`
# lexicals. The sub is installed into the package at compile time, so it is
# callable via OUR:: before its declaring block has run (returning the
# undefined value), and returns the captured value once the block has run.
# https://github.com/Raku/old-issue-tracker/issues/1908

plan 6;

{
    nok &OUR::access_lexical_a().defined,
        'our-sub captured lexical is undefined before the block runs';
    {
        my $a = 42;
        our sub access_lexical_a() { $a }
    }
    is &OUR::access_lexical_a(), 42,
        'our-sub returns captured lexical value after the block runs';
}

# A sibling block declaring a same-named `my` lexical must NOT pollute the
# captured value (same-named locals share a slot in the VM).
{
    {
        my $b = 7;   # unrelated, not captured by an our-sub
    }
    nok &OUR::grab_b().defined,
        'unrelated sibling my $b does not leak into the captured cell';
    {
        my $b = 99;
        our sub grab_b() { $b }
    }
    is &OUR::grab_b(), 99, 'our-sub captures its own block lexical';
}

# An `our sub` reading an `our` package var still resolves through the package
# store (not the escaping-lexical mechanism).
{
    package Gee {
        our $msg;
        our sub talk { $msg }
    }
    $Gee::msg = "hello";
    is Gee::talk, "hello", 'our-sub still reads our-vars via the package store';
}

# Multiple captured lexicals in one block.
{
    {
        my $x = 1;
        my $y = 2;
        our sub sum_xy() { $x + $y }
    }
    is &OUR::sum_xy(), 3, 'our-sub captures multiple block lexicals';
}
