use Test;

# A sigilless binding (`my \i`, `-> \i { }`) shadows the `i` term constant
# (the imaginary unit) inside its scope, just as `\e`/`\pi` shadow those
# constants. A same-named `$`-scalar (`my $i`, `-> $i`) does NOT shadow the
# term. Regression pins for Language/py-nutshell.rakudoc doc-diff findings.
#
# NOTE: the bare-`i`-is-imaginary checks come first on purpose. A source-level
# `my \i` registers `i` as a user term symbol at parse time, and that
# registration currently leaks past its block scope (a separate pre-existing
# parser-scope bug), so a later bare `i` in the *same compilation unit* would
# be mis-parsed. Ordering the negative checks first keeps them unaffected.

plan 9;

# The bare term constant is the imaginary unit when no sigilless binding is in
# scope, and a same-named `$`-scalar must NOT shadow it.
is i,   0+1i, 'bare i is the imaginary unit';
is 3i,  0+3i, '3i literal still parses';
{
    my $i = 5;
    is i, 0+1i, 'a $-scalar my does not shadow imaginary i';
}
{
    my $b = -> $i { i };
    is $b(5), 0+1i, 'a $-scalar pointy param does not shadow imaginary i';
}

# Single sigilless pointy parameter named i shadows the constant.
{
    my $b = -> \i { i * 2 };
    is $b(9), 18, 'sigilless pointy param \i shadows imaginary i';
    is ( -> \i { i * 2 } for 3, 9 ).List, (6, 18), 'sigilless \i in a for-block expression';
}

# Other sigilless names keep working (non-constant).
{
    my $b = -> \v { v * v };
    is $b(6), 36, 'sigilless pointy param with a non-constant name';
}

# my \i shadows the constant when captured in a nested closure and read directly.
{
    my \i = 5;
    my $b = { i * 2 };
    is $b(), 10, 'my \i shadows imaginary i inside a nested closure';
}
{
    my \i = 7;
    is i * 3, 21, 'my \i shadows imaginary i in the same scope';
}
