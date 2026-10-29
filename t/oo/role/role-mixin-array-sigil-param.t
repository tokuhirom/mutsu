use Test;

# Data::Dump::Tree (MultiColumns `get_columns`): an itemized Array carrying a
# role mixin (`[...] but R`), bound to an `@` parameter, is the Array itself.
plan 6;

role M { has $.n is rw = 0 }
my $m = [5, 6, 7] but M;

sub elems(@c) { @c.elems }
is elems($m), 3, '@ param over a mixin Array has its elements';

sub triples(@c) {
    my @seen;
    for @c -> $x, $y, $z { @seen.push: "$x $y $z" }
    @seen.join('|');
}
is triples($m), '5 6 7', 'for @param iterates a mixin Array element-wise';

sub each(@c) {
    my @seen;
    for @c { @seen.push: $_ }
    @seen.join(',');
}
is each($m), '5,6,7', 'for @param with the topic';

my @blocks;
my $block = [] but M;
$block.n max= 3;
$block.push: 1, [2, 3], 'x';
@blocks.push: $block;
my @got;
for @blocks -> @b {
    for @b -> $a, $b, $c { @got.push: "$a/$b.join('+')/$c" }
}
is @got.join(','), '1/2+3/x', 'nested pointy loop over a mixin Array element';
is $block.n, 3, 'the mixin state survives the bind';

sub same(@c) { @c === $m }
ok same($m), 'the @ param is the same Array object';
