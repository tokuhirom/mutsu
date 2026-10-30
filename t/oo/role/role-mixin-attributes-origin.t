use Test;

# From Data::Dump::Tree 2.9.4 (t/06_role_attributes): it marks an attribute
# with the origin ` +{Role}` when the attribute's `.package` name ends in
# `+{...}`. An attribute a role contributes to a run-time mixin belongs to the
# mixin's anonymous class (`Int+{E}`), not to the role.

plan 5;

class Fruit { has $.seeds }
role Tomato { has $.color = 'red' }
role E { has $.fruit = 1 }

my $m = 42 but E;
my $fruit = $m.^attributes.first(*.name eq '$!fruit');
is $fruit.package.^name, 'Int+{E}', 'a role attribute of "42 but E" is packaged by the mixin';

my $f = Fruit.new(:seeds(1)) but Tomato;
my %by-name = $f.^attributes(:all).map({ .name => .package.^name });
is %by-name<$!color>, 'Fruit+{Tomato}', 'the role attribute of a mixed-in class instance';
is %by-name<$!seeds>, 'Fruit', 'a class attribute keeps its class';

my $marker = $fruit.package.^name ~~ / ( '+' <-[+]> * ) $/ ?? " $0" !! '';
is $marker, ' +{E}', "Data::Dump::Tree's origin marker is found";

# The value stays readable through the attribute meta-object.
is $fruit.get_value($m), 1, 'get_value still reads the mixed-in attribute';
