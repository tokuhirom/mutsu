use Test;

plan 6;

# `Attribute.set_value` binds the container it is given: the object's slot and
# the caller's variable are one container afterwards.
class Foo { has $.attr is rw; }
my $attr = Foo.^attributes.head;
sub bind-it($obj, Mu $v is raw) { $attr.set_value($obj, $v) }

my $o = Foo.new;
my $c = 5;
bind-it($o, $c);
is $o.attr, 5, 'the bound value is read through the accessor';
$o.attr = 6;
is $c, 6, 'assigning through the accessor reaches the caller variable';
$c = 7;
is $o.attr, 7, 'and the other way round';

# An alias of the accessor writes the same container.
Foo.^add_method('nick', Foo.^method_table<attr>);
$o.nick = 8;
is $o.attr, 8, 'assigning through an alias of the accessor';
is $c, 8, 'reaches the caller variable too';

# A plain value is still bound as a value.
my $p = Foo.new;
$attr.set_value($p, 3);
is $p.attr, 3, 'a plain value is stored';
