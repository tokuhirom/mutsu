use Test;

# A metaobject (`.HOW`) is canonical per introspected type: `1.HOW` and
# `2.HOW` are the same ClassHOW, so `===` compares them by the introspected
# type, not by a fresh allocation. And `$how.name($obj)` is a ClassHOW method
# (`$obj.^name` desugars to `$obj.HOW.name($obj)`), not an rw-accessor write.

plan 12;

# HOW identity (=== over metaobjects)
ok  1.HOW === 2.HOW,   'same-type instances share the metaobject';
ok  1.HOW === Int.HOW, 'instance and type object share the metaobject';
nok 1.HOW === Num.HOW, 'different types have distinct metaobjects';
ok  "a".HOW === "b".HOW, 'two Str instances share the Str metaobject';
nok "a".HOW === 1.HOW,   'Str and Int metaobjects differ';

# .name($obj) on a metaobject, inline chain
is 1.HOW.name(1), 'Int', 'inline 1.HOW.name(1) is Int';
is 1.^name,       'Int', '1.^name is Int';

# .name($obj) on a metaobject held in a variable (was misread as an rw setter)
my $object     = 1;
my $metaobject = 1.HOW;
is $metaobject.name($object), 'Int', 'variable $metaobject.name($object) is Int';

my $h = Int.HOW;
is $h.name(Int), 'Int', 'variable Int.HOW held, .name(Int) is Int';

# Works for a user class too
class Foo { has $.x }
my $fh = Foo.HOW;
is $fh.name(Foo), 'Foo', 'user-class metaobject .name is Foo';
ok Foo.HOW === Foo.new.HOW, 'user-class type object and instance share metaobject';
nok Foo.HOW === Int.HOW,    'user-class and Int metaobjects differ';
