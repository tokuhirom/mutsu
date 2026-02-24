use Test;

plan 6;

dies-ok { Supply.map({ ... }) }, 'Supply.map cannot be called as a class method';

my $mapped = Supply.from-list(1..3).map(* * 2);
isa-ok $mapped, Supply, 'Supply.map returns a Supply';
is $mapped.live, False, 'mapped supply is on-demand (not live)';

my @scaled;
$mapped.tap(-> $v { @scaled.push($v) });
is-deeply @scaled, [2, 4, 6], 'mapped values are emitted through tap';

my @wrapped;
Supply.from-list(1..3).map({[$_]}).tap(-> $v { @wrapped.push($v) });
is-deeply @wrapped, [[1], [2], [3]], 'mapping arrays does not flatten results';

my @listish;
Supply.from-list([1,2], [3,4,5]).map({ .reverse.Str }).tap(-> $v { @listish.push($v) });
is-deeply @listish, ["2 1", "5 4 3"], 'from-list preserves explicit arrays as supply items';
