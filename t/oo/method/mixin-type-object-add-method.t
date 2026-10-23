use Test;

# The type object of `Class.^mixin(R)` is a real class: the metamethods that read
# a type's members (`method_table`, `add_method`, `can`) see its own table, and
# a method added to it is called on the instances `.new` makes (#13264,
# AttrX::Mooish's alias accessors).

plan 8;

role R { has $.attr is rw = 1; method hi { "hi" } }
my class Foo { }
my $m := Foo.^mixin(R);

ok $m.^method_table<attr>.defined, 'the accessor the role gives the mixin class is in its method table';
$m.^add_method('nick', $m.^method_table<attr>);
$m.^add_method('hello', method { "hello" });
ok $m.^can('nick'), '^can sees an alias added to the mixin class';
ok $m.^can('hello'), '^can sees a method added to the mixin class';
ok $m.^can('hi'), '^can still sees the role method';
nok Foo.^can('hello'), 'the base class does not get it';

my $o = $m.new;
is $o.hello, 'hello', 'the added method is called on an instance';
$o.nick = 7;
is $o.attr, 7, 'assigning through the alias reaches the rw attribute';
is $o.nick, 7, 'reading through the alias';
