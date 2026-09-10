use Test;

plan 5;

dies-ok { Supply.delayed(1) }, "Supply.delayed cannot be called on the type object";

my $source = Supply.from-list(1..3);
my $noop = $source.delayed(0);
ok $source === $noop, "Supply.delayed(0) is identity";

my @noop_values;
$noop.tap(-> $value { @noop_values.push($value) });
is-deeply @noop_values, [1, 2, 3], "identity delayed supply preserves emitted values";

my $delayed = $source.delayed(0.01);
ok $source !=== $delayed, "positive delay returns a derived Supply";

my @delayed_values;
$delayed.tap(-> $value { @delayed_values.push($value) });
is-deeply @delayed_values, [1, 2, 3], "delayed supply preserves emitted values";
