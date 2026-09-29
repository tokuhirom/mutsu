use Test;

# From the Commands ecosystem distribution: a statement whose last term is a
# pair with a block value ends at that block's closing brace at end of line,
# so the next line's `if COND -> $x { }` is a statement, not a modifier.

plan 3;

my @a;
my %al = q => [1];
my $seen;
@a.push: 1 => { 1 }
if %al<q> -> $alias { $seen = $alias }
is-deeply $seen, [1], 'pointy if after a pair-block statement runs';

my @b;
@b.push: 2 => { 2 }
if %al<q> { @b.push: 3 }
is @b.elems, 2, 'plain if after a pair-block statement is its own statement';

my @c = <a b>.map: -> $k {
    my @x;
    @x.push: $k => { 1 }
    if %al<q> -> $al { @x.push: $al }
    @x.Slip
};
is @c.elems, 4, 'inside a pointy block';
