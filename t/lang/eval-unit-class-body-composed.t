use Test;
use MONKEY-SEE-NO-EVAL;

plan 6;

# The body statements of an EVAL'd `unit class` run after the class is
# composed, and they write the caller's lexicals.
my ($methods, $composed);
EVAL Q[unit class P; has $.name; $composed = P.^is_composed; $methods = P.^methods(:local).map(*.name).sort.join(',')];
ok $composed, 'unit class body statements see the composed class';
is $methods, 'POPULATE,name', 'and its generated methods';

my $saw;
EVAL Q[unit class Foo; $saw = Foo.^is_composed];
ok $saw, 'a write to the caller lexical from the unit class body survives';

my ($a, $b);
EVAL Q[class P2 { $a = 5 }; class P3 { $b = 6 }];
is "$a $b", '5 6', 'class bodies in an EVAL write caller lexicals';

# An `our` of the class body stays a package variable.
EVAL Q[unit class Q1; our $v = 7;];
is $Q1::v, 7, 'an our variable of the unit class body is the package variable';

# A variable trait handled by a trait_mod declared in the same EVAL unit.
my @log;
EVAL Q[sub trait_mod:<is>(Variable:D $v, :$logged!) { @log.push("trait") }; @log.push("first"); my $x is logged = 1; @log.push("last")];
is @log.join(','), 'trait,first,last', 'variable traits are applied in the declaration phase';
