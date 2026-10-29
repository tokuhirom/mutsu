use Test;

plan 8;

# A class declared in term position (`my $x = class Foo { ... }.^name`) is a
# compile-time declaration like the statement form: it completes an earlier
# stub and applies its attribute traits before the unit's run-time code runs.
my @applied;
multi sub trait_mod:<is>(Attribute:D $a, :$col!) { @applied.push($a.name) }

class Stubbed { ... }
is @applied.elems, 2, 'both declarations already applied their attribute traits';
is Stubbed.^attributes.elems, 1, 'the stub is complete before its declaration statement runs';
my $name = class Stubbed { has $.a is col; method hi { 'hi' } }.^name;
is $name, 'Stubbed', 'the declaration in term position still evaluates to the type';
is @applied.elems, 2, 'no trait ran twice';
is Stubbed.new(a => 3).a, 3, 'the class works';
is Stubbed.hi, 'hi', 'its method is there';

# The statement form behaves the same.
class Other { ... }
is @applied.elems, 2, 'before: statement form';
class Other { has $.b is col }
is @applied.elems, 2, 'after: statement form registers its trait once';
