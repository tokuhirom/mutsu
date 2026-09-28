use Test;

# `Any.Slip` is `self.list.Slip`, so a Set, Bag, Mix or Hash slips its pairs.
# mutsu wrapped the whole container in a one-element slip, so an empty set
# difference counted as one element: Log::Reader's `t/01-tests.t` checks
# `+($e.keys.sort (-) $g.keys.sort).Slip == 0`.

plan 8;

is-deeply set(1).Slip, (1 => True,).Slip, 'Set.Slip slips its pairs';
is-deeply bag(1, 1).Slip, (1 => 2,).Slip, 'Bag.Slip slips its pairs';
is-deeply (a => 0.5).Mix.Slip, ((a => 0.5),).Slip, 'Mix.Slip slips its pairs';
is-deeply {a => 1}.Slip, ((a => 1),).Slip, 'Hash.Slip slips its pairs';

is +set().Slip, 0, 'an empty Set slips nothing';
is +{}.Slip, 0, 'an empty Hash slips nothing';

my %e = a => 1, b => 2;
my %g = b => 3, a => 4;
ok +(%e.keys.sort (-) %g.keys.sort).Slip == 0, 'empty set difference slips to zero elements';
is +(set(<a b c>) (-) set(<a>)).Slip, 2, 'non-empty set difference slips its elements';
