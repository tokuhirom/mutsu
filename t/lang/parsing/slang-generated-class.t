use v6.e.PREVIEW;
use lib 't/lib';
use Test;

# A slang production that builds a class with the metamodel and installs it for
# the importing unit (#13370, ADR-13370): the class works in the unit, with the
# methods, attribute and role the production gave it.

plan 7;

use GeneratedClassSlang;

defclass Tally 5;
my $t = Tally.new;
is $t.value, 5, 'a method the production added runs in the unit';
is $t.bump, 5, 'a method using closure state';
is $t.bump, 10, 'the closure state accumulates';
is $t.greet, 'hello from Tally', 'the composed role supplies a method';
ok Tally ~~ Greeter, 'the type does the composed role';
is Tally.^name, 'Tally', 'the class keeps its name';

defclass Other 7;
is Other.new.value + Tally.new.value, 12, 'a second production installs a second class';
