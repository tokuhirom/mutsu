use Test;

# An anonymous role (`role { ... }`) mixed into a class with `.^mixin` or into
# an object with `but` exposes its attributes and methods to introspection, and
# its attribute traits run when the mixin class is made (#13264).

plan 9;

our @ran;
multi trait_mod:<is>(Attribute:D $a, :$tr!) { @ran.push($a.name) }

class Foo { has $.x is tr; }
my $r = role { has $!b is tr; has $!c; method hi { "hi" } };

is-deeply $r.^attributes.map(*.name).List, ('$!b', '$!c'),
    'an anonymous role lists its own attributes';
is-deeply $r.^methods.map(*.name).List, ('hi',),
    'an anonymous role lists its own methods';

my $m = Foo.^mixin($r);
is-deeply $m.^attributes.map(*.name).List, ('$!b', '$!c', '$!x'),
    'the mixin type lists the role attributes before the class attribute';
ok $m.^can('hi'), 'the mixin type has the role method';

my $o = Foo.new but $r;
is-deeply $o.^attributes.map(*.name).List, ('$!b', '$!c', '$!x'),
    'an object mixed in with `but` lists the role attributes';
is $o.hi, 'hi', 'the role method is callable';

role Named { has $!n is tr; }
my $named = Foo.^mixin(Named);
is-deeply $named.^attributes.map(*.name).List, ('$!n', '$!x'),
    'a named role: the mixin type lists the attributes';

my $second = Foo.^mixin(Named);

my $inline = Foo.^mixin(role { has $!z is tr });
is-deeply $inline.^attributes.map(*.name).List, ('$!z', '$!x'),
    'an inline role expression passed to .^mixin';
is-deeply @ran.sort.List, ('$!b', '$!n', '$!x', '$!z'),
    'each attribute trait ran exactly once, whatever number of mixins used the role';
