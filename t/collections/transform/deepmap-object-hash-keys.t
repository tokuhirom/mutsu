use Test;

# From ML::TriesWithFrequencies (t/11-classifications): `categorize` returns an
# object hash; deepmap/duckmap/nodemap over it used to surface the internal
# `Str|a` WHICH keys instead of the real keys.

plan 5;

my @res = "a" => 1, "b" => 2, "a" => 3;
my %h = @res.categorize({ $_.key }).deepmap({ $_.value }).map({ .key => .value.sum });
is-deeply %h, {a => 4, b => 2}, 'deepmap over categorize keeps real keys';

my %c = @res.categorize({ $_.key });
is %c.deepmap({ $_.value }).keys.sort.join(','), 'a,b', 'deepmap keys';
is %c.duckmap({ .elems }).keys.sort.join(','), 'a,b', 'duckmap keys';
is %c.nodemap({ .elems }).keys.sort.join(','), 'a,b', 'nodemap keys';

my %o{Any} = a => 1;
is %o.deepmap({ $_ + 1 }).keys.join(','), 'a', 'deepmap over a %h{Any} object hash';

done-testing;
