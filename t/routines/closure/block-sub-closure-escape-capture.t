use Test;

# A closure that escapes a block calls the block's lexical sub through the
# sub's escape-hatch value. That value is captured the way any closure is
# (the sub's free variables plus the enclosing scope's memoized layers), not
# as a second handle on the live env chain, which made every write-back on
# the block's exit copy the whole enclosing scope (#9170). The sub still sees
# what its block declared, and what the enclosing scope holds.

plan 6;

my $outer = 'outer';
my &f;
{
    my $inner = 'inner';
    sub reads-both { "$inner/$outer" }
    &f = -> { reads-both() };
}
is f(), 'inner/outer', 'the escaped sub reads its block lexical and the outer one';
$outer = 'changed';
is f(), 'inner/changed', 'an outer write after the block is seen';

my &g;
{
    my $count = 0;
    sub bump { ++$count }
    &g = -> { bump() };
}
g(); g();
is g(), 3, 'the escaped sub keeps updating its block lexical';

my &h;
{
    sub helper-a { 'a' }
    sub helper-b { helper-a() ~ 'b' }
    &h = -> { helper-b() };
}
is h(), 'ab', 'an escaped sub calls a sibling sub of its block';

my @calls;
for ^3 -> $i {
    my $tag = "t$i";
    sub tagged { $tag }
    @calls.push: -> { tagged() };
}
is @calls.map({ $_() }).join(','), 't0,t1,t2', 'each iteration escapes its own sub';

my $t = 0;
for ^50 { { sub one { 1 }; $t += one() } }
is $t, 50, 'a block that declares a sub writes back the outer variable';
