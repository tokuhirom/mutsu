use v6;
use lib 't/lib/ResSplit/lib';
use Test;

plan 4;

# Found via the Intl::CLDR ecosystem distribution: a `%?RESOURCES` entry is a
# `Distribution::Resource`, which binds to that type and whose `.split` splits
# the file CONTENT (mutsu split the path string and rejected the binding).
use ResSplit;

my $r = strs();
sub take(Distribution::Resource :$f) { $f.split(31.chr).list }

ok $r ~~ Distribution::Resource, 'a %?RESOURCES entry is a Distribution::Resource';
nok IO::Path.new('a/b') ~~ Distribution::Resource, 'a plain IO::Path is not';
is-deeply take(f => $r), <a b c>.list, '.split on a named-parameter receiver splits the content';
is-deeply $r.split(31.chr).list, <a b c>.list, '.split on a call result splits the content';
