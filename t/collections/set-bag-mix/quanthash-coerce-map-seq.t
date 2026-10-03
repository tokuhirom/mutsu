use Test;

plan 7;

# The bareword coercers read a still-deferred `.map`/`.grep` Seq's elements.
is-deeply Bag(<x y>.map({ $_ => 2 })), (x => 2, y => 2).Bag, 'Bag(map Seq of pairs)';
is-deeply Bag(<x y x>.map({ $_ })), <x y x>.Bag, 'Bag(map Seq)';
is-deeply Set(<x y>.grep({ $_ })), <x y>.Set, 'Set(grep Seq)';
is-deeply Mix(<x y>.map({ $_ => 2 })), (x => 2, y => 2).Mix, 'Mix(map Seq of pairs)';
is BagHash(<x y>.map({ $_ })).elems, 2, 'BagHash(map Seq)';
is-deeply Hash(<x y>.map({ $_ => 1 })), {x => 1, y => 1}, 'Hash(map Seq of pairs)';
my @rows = ('Cat', 'Tom', 3), ('Dog', 'Rex', 2), ('Cat', 'Kit', 1);
my %by = @rows.classify({ $_[0] }).map({ .key.lc => Bag(.value.map({ $_[1] => $_[2] })) });
is-deeply ([(+)] %by.values), (Kit => 1, Rex => 2, Tom => 3).Bag, 'Bag built from rows per species';
