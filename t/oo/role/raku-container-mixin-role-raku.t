use Test;

plan 6;

# A role's own `raku` method is used for a mixed-in value inside a container,
# as it is for the value on its own.
my role T { has $.type; method raku(T:D:) { callsame() ~ " but T('$!type')" } }
my $x = "foo" but T<a>;
is $x.raku, q{"foo" but T('a')}, 'the value on its own';
is ($x,).raku, q{("foo" but T('a'),)}, 'in a List';
is (:not($x)).raku, q{:not("foo" but T('a'))}, 'as a Pair value';
is [$x, 1].raku, q{["foo" but T('a'), 1]}, 'in an Array';

my role R { }
is (5 but R,).raku, '(5,)', 'a role without its own raku renders the value';
is [1, <3>].raku, '[1, IntStr.new(3, "3")]', 'an allomorph is unchanged';
