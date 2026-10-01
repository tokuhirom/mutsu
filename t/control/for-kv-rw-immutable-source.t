use Test;
plan 4;

my Map $m = Map.new((a => 1));
throws-like { for $m.kv -> $k, $v is rw { $v = 5 } }, X::Parameter::RW,
    'immutable Map .kv: is rw bind fails';
my $mix = Mix.new(<a b>);
throws-like { for $mix.kv -> $k, $v is rw { } }, X::Parameter::RW,
    'immutable Mix .kv: is rw bind fails even without assignment';
my %h = a => 1;
for %h.kv -> $k, $v is rw { $v = 5 }
is %h<a>, 5, 'mutable Hash .kv: is rw still aliases';
my $bh = BagHash.new(<a>);
for $bh.kv -> $k, $v is rw { $v = 3 }
is $bh<a>, 3, 'mutable BagHash .kv: is rw still aliases';
