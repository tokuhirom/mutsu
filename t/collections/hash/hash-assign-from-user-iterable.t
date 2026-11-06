use Test;

plan 5;

# A user class that is Iterable through its own `iterator` contributes what
# that iterator yields when it is assigned to a `%` variable (it used to
# become one stringified key).
class It does Iterator {
    has $.n = 0;
    method pull-one { $!n < 3 ?? ($!n++ => $!n * 2) !! IterationEnd }
}
class Se does Iterable { method iterator { It.new } }

my %a = Se.new;
is-deeply %a, {"0" => 2, "1" => 4, "2" => 6}, 'plain hash';
my %m is Map = Se.new;
is-deeply %m, Map.new(("0" => 2, "1" => 4, "2" => 6)), 'is Map';
my %h is Hash = Se.new;
is %h.elems, 3, 'is Hash';
my @l = Se.new;
is @l.elems, 3, 'the array form still iterates';
my %again = Se.new;
is %again<2>, 6, 'a fresh iterator each time';

# vim: expandtab shiftwidth=4
