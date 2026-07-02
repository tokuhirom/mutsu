use Test;

# §1.5 slice S8: `$x does Role` on a named variable writes the mixed-in value
# back through the compile-time-baked local slot (DoesVar), not a by-name
# `code.locals` search.

plan 6;

role Greet { method greet { 'hi' } }

# basic `does` on a local variable
{
    my $x = 42;
    $x does Greet;
    is $x.greet, 'hi', 'the mixed-in role method is callable';
    is $x, 42, 'the underlying value is preserved';
}

# a shadowing inner-block `does` mixes into the inner binding; outer is intact
{
    my $x = 1;
    {
        my $x = 2;
        $x does Greet;
        is $x.greet, 'hi', 'inner shadow gets the role';
        is $x, 2, 'inner value preserved';
    }
    ok !($x ~~ Greet), 'the outer binding did not get the role';
}

# a `submethod TWEAK` run by the mixin can mutate a captured-outer lexical
{
    my $n = 0;
    role Counter { submethod TWEAK { $n++ } }
    my $x = 1;
    $x does Counter;
    is $n, 1, 'captured-outer lexical mutated by the mixin TWEAK';
}
