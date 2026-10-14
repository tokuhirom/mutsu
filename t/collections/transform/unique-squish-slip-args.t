use Test;

# A Slip argument slips into a non-flattening `**@` slurpy and into the
# routine forms of unique/repeated/squish (`+values`), while every other
# iterable argument stays one element (#13506; PDF::COS::Tie::Hash.check runs
# `unique(self.keys.Slip, %!required-entries.keys.sort.Slip)`).

plan 9;

sub g(**@a) { @a }
is-deeply g((1, 2).Slip, 3), [1, 2, 3], '**@ slips a Slip argument';
my $s = (1, 2).Slip;
is-deeply g($s), [1, 2], 'a Slip held in a scalar slips too';
is-deeply g(Empty, 1), [1], 'Empty contributes nothing';
is-deeply g([1, 2], 3), [[1, 2], 3], 'an Array argument stays one element';

is-deeply unique((1, 2).Slip, (2, 3).Slip), (1, 2, 3), 'unique slips its Slip arguments';
is-deeply unique((1, 1).Slip), (1,), 'unique of one Slip';
is-deeply squish((1, 1).Slip, 2), (1, 2), 'squish slips its Slip arguments';
is-deeply repeated((1, 1).Slip, 2), (1,), 'repeated slips its Slip arguments';
is-deeply unique([1, 1, 2]), (1, 2), 'a single Array is still the list';
