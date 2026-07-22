use v6;
use Test;

plan 5;

# A Supply cannot be constructed directly (PLAN.md §8.15): Rakudo throws
# X::Supply::New pointing at Supplier / Supply.on-demand / supply blocks.

throws-like { Supply.new }, X::Supply::New,
    message => /'Cannot directly create a Supply'/,
    'Supply.new throws X::Supply::New';

# The supported construction paths still work.
my $sup = Supplier.new;
isa-ok $sup.Supply, Supply, 'Supplier.Supply gives a live supply';
isa-ok 1.Supply, Supply, '.Supply coercion works';

my $sum = 0;
my $s = $sup.Supply;
$s.tap(-> $x { $sum += $x });
$sup.emit(2);
$sup.emit(3);
is $sum, 5, 'live supply taps still receive values';

my @got;
(supply { emit 7; emit 8 }).tap(-> $x { @got.push($x) });
is-deeply @got, [7, 8], 'supply blocks still work';
