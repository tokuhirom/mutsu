use v6;
use lib 't/lib/ResSplit/lib';
use Test;

plan 4;

# From CSS::TagSet: `sub f(IO() $css)` given a `%?RESOURCES` entry coerces it to
# the IO::Path of its file (it died with "Impossible coercion from
# 'Distribution::Resource' into 'IO'").
use ResSplit;

sub f(IO() $x) { $x }
my $r = strs();

isa-ok f($r), IO::Path, 'IO() coerces a resource to an IO::Path';
is f($r).basename, 'strs.bin', 'it names the resource file';
is f($r).slurp, $r.slurp, 'the same content';
my IO() $y = $r;
isa-ok $y, IO::Path, 'the same for a declaration';
