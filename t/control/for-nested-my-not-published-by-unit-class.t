use v6;
use lib 't/lib-nested-block-my-leak';
use Leak2::Sub;
use Leak2::Sub::Walker;
use Test;

# A `my` declared in a block nested at the top of a `use`d `unit class` file
# (`for @rows { my $op = $_ }`) kept its last value bound in the loader's env,
# and the enclosing `unit class` published it as a static: a fresh `my $op;`
# in code that called into that namespace then read the stale value. Found
# with Math::Symbolic (`.expand` rendered `a*(a*a)`). Identical under raku.

plan 3;

my $walker = Leak2::Sub::Walker.new;
{
    my $op;
    $walker.walk(:type<x>);
    is $op.raku, 'Any', 'a fresh `my $op` stays unset across a method call';
}
{
    my $op = 7;
    $walker.walk(:type<x>);
    is $op, 7, 'an assigned variable keeps its value';
}
{
    my $rows;
    is $rows.raku, 'Any', 'a top-level static name is unaffected';
}
