unit module EnumParamUser;
use EnumParamDefs :Kinds;

our sub mapped(@a) { @a.map( -> $literal { :$literal }).List }
our sub mapped-plain(@a) { @a.map( -> $literal { $literal }).List }
our sub mapped-key(@a) { @a.map( -> $dict { $dict ~ '!' }).List }
our sub direct($x) { my $c = -> $literal { $literal }; $c($x) }
our sub looped(@a) { my @r; for @a -> $array { @r.push($array) }; @r }
our sub as-term() { literal }
our sub term-in-map(@a) { @a.map({ literal }).List }
our sub scalar-and-term() { my $literal = 'mine'; ($literal, literal) }
