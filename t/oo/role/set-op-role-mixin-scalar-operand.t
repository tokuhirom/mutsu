use Test;

# A role mixed into a scalar (here a live Attribute meta-object, as Red's
# `$attr does Red::Attr::Relationship[...]` does) must survive being an operand
# of a set operator: the element keeps its mixin and so its methods (#12532).

plan 7;

role R { method hello($x) { "hello $x from " ~ self.name } }
class C { has $.a; }

my $attr = C.^attributes[0];
$attr does R;

is set($attr).keys[0].hello("set"), 'hello set from $!a', 'set() keeps the mixin';
is (set() (|) $attr).keys[0].hello("rhs"), 'hello rhs from $!a', 'scalar on the right of (|)';
is ($attr (|) set()).keys[0].hello("lhs"), 'hello lhs from $!a', 'scalar on the left of (|)';

my %h{Attribute};
%h (|)= $attr;
is %h.keys[0].hello("assign"), 'hello assign from $!a', 'object hash updated with (|)=';
ok %h.keys[0] ~~ R, 'the key still does the role';

# A role mixin over an aggregate is still folded through, as before.
my %plain = a => 1;
%plain does R;
is-deeply (%plain (|) set()).keys.sort.List, ('a',), 'role-mixed hash still flattens';
role R2 { }
my $n = 5 but R2;
is (set() (|) $n).elems, 1, 'a role-mixed Int is one element';
