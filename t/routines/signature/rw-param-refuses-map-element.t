use Test;

# An immutable Map's element is a value, not a container: passed straight to
# an `is rw` parameter it is refused with X::Parameter::RW, and the Map is
# left alone (#17954). A Hash element is a container and is written.

plan 7;

sub isrw($x is rw) { $x = 7 }

my $m = Map.new((a => 1));
throws-like { isrw($m<a>) }, X::Parameter::RW, 'a Map element is refused';
is-deeply $m, Map.new((a => 1)), '... and the Map is unchanged';
throws-like { isrw($m<zz>) }, X::Parameter::RW, 'a missing Map key is refused too';

my %im is Map = a => 1;
throws-like { isrw(%im<a>) }, X::Parameter::RW, 'an `is Map` variable\'s element as well';

sub raw-kind($x is raw) { $x.VAR.^name }
is raw-kind($m<a>), 'Int', 'an is raw parameter gets the Map element\'s value';

my %h = a => 1;
isrw(%h<a>);
is %h<a>, 7, 'a Hash element is written through an is rw parameter';
is raw-kind(%h<a>), 'Scalar', '... and is a container to an is raw parameter';
