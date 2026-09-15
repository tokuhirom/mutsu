use Test;

# Found in P5each: a while condition's pair must be unpacked into its
# destructuring pointy parameter on every iteration.
plan 2;

my @items = <a b c>;
my $index = 0;
my @keys;
my @values;

sub next-pair() {
    return Empty unless $index < @items.elems;
    my $current = $index++;
    ($current, @items[$current])
}

while next-pair() -> ($key, $value) {
    @keys.push($key);
    @values.push($value);
}

is-deeply @keys, [0, 1, 2], 'while destructures the condition pair keys';
is-deeply @values, [<a b c>], 'while destructures the condition pair values';
