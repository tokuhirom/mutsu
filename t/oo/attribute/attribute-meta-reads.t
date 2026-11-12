use Test;

# The reading metamethods of an `Attribute` from `.^attributes`, called on
# every attribute of a class, as a state copier walks them.

plan 14;

class P { has Int $.a = 1; has @!l = 1, 2; has %.h; has $.r is rw is required = 3 }
my $p = P.new(:r(5));
my @attrs = P.^attributes;

is @attrs.map(*.name).join(' '), '$!a @!l %!h $!r', '.name';
ok @attrs.map(*.package).all === P, '.package';
is @attrs.map({ .type.^name }).join(' '), 'Int Positional Associative Mu', '.type';
is @attrs.map(*.has_accessor).join(' '), 'True False True True', '.has_accessor';
is @attrs.map(*.rw).join(' '), 'False False False True', '.rw';
is @attrs.map(*.readonly).join(' '), 'True True True False', '.readonly';
ok @attrs[3].required, '.required';
is-deeply @attrs.map({ .get_value($p) }).List[0, 3], (1, 5), '.get_value of scalars';
is-deeply @attrs[1].get_value($p).List, (1, 2), '.get_value of an array';
is @attrs.grep({ .name eq any('$!a', '$!r') }).elems, 2, 'name eq any(...)';

class Q is P {}
is @attrs[0].get_value(Q.new(:a(9), :r(1))), 9, '.get_value of a subclass instance';
is @attrs[0].get_value(|($p,)), 1, 'a spread argument';
my $m = 'name';
is @attrs[0]."$m"(), '$!a', 'a late-bound method name';
my $slot := @attrs[0].get_value($p);
$slot = 42;
is $p.a, 42, '.get_value hands out the container';
