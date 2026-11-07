use v6;
use lib 't/lib-for-multi-param-slot-leak';
use Leak::Sub;
use Leak::Sub::Walker;
use Test;

# The parameters of a multi-parameter `for` (`-> $i, $var`) nested in another
# loop at the top of a `use`d `unit class` file kept their last values in their
# local slots after the loop, and the class body published them as statics of
# the enclosing `unit class`. A later fresh `my $i;` in code that called into
# that namespace then read the stale value. Found with Math::Symbolic
# (`expand` took the wrong branch on `$i == 1`). Identical under raku.

plan 3;

my $walker = Leak::Sub::Walker.new;
{
    my $i;
    $walker.walk(:type<x>);
    is $i.raku, 'Any', 'a fresh `my $i` stays unset across a method call';
}
{
    my $var;
    $walker.walk(:type<x>);
    is $var.raku, 'Any', 'and so does `$var`';
}
{
    my $i = 7;
    $walker.walk(:type<x>);
    is $i, 7, 'an assigned variable keeps its value';
}
