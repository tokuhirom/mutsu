use Test;

# The class method `Supply.merge(*@s)` flattens an array of supplies into the
# supplies themselves. Tinky's `enter-supply` merges one mapped supply per
# state with `Supply.merge(@supplies)`, which died with "Can only merge Supply
# objects, got Array".

plan 4;

my @s = Supply.from-list(1, 2), Supply.from-list(3);
is Supply.merge(@s).list.sort, (1, 2, 3), 'an array of supplies merges its elements';
isa-ok Supply.merge(@s), Supply, 'the result is a Supply';

my $a = Supplier.new;
my $b = Supplier.new;
my @live = $a.Supply, $b.Supply;
my @seen;
Supply.merge(@live).tap: { @seen.push: $_ };
$a.emit('a');
$b.emit('b');
is @seen, <a b>, 'live supplies passed as an array stay live';

throws-like { Supply.merge([1, 2]) }, X::Supply::Combinator,
    'a non-Supply element is still rejected';
