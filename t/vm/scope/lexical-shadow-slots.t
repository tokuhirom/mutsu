use Test;

# §1.4 lexical shadow slots. These pass in the default build (shadowing correct
# via the runtime BlockScope restore) AND under MUTSU_SHADOW_SLOTS=1 (distinct
# compile-time slots). They pin the shadow-slot activation machinery in
# src/compiler/mod.rs (declare_local / pop_local_scope). See
# docs/lexical-scope-slot-campaign.md.

plan 9;

# Simple one-level shadow: inner `my $x` does not disturb the outer binding.
my $x = 1;
{
    my $x = 2;
    is $x, 2, 'inner shadow reads its own value';
    $x++;
    is $x, 3, 'inner shadow mutation stays inner';
}
is $x, 1, 'outer value restored after block';

# Nested shadows: each level has its own binding.
my $y = 10;
{
    my $y = 20;
    {
        my $y = 30;
        is $y, 30, 'innermost shadow';
    }
    is $y, 20, 'middle shadow restored after innermost block';
}
is $y, 10, 'outermost restored after all blocks';

# Sibling blocks each shadow independently and do not leak between each other.
my $z = 100;
{
    my $z = 1;
    is $z, 1, 'first sibling shadow';
}
{
    my $z = 2;
    is $z, 2, 'second sibling shadow independent of the first';
}
is $z, 100, 'outer restored after both sibling blocks';
