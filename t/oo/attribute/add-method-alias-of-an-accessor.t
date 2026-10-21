use Test;

plan 7;

# `$class.^add_method($alias, $class.^method_table<attr>)` installs the
# attribute's accessor under another name (AttrX::Mooish's `:alias`): the alias
# reads the attribute, and an `is rw` accessor stays assignable (#14266).
class Foo {
    has $.val = 5;
    has $.rw is rw = 1;
    has Int $.typed = 3;
}
BEGIN {
    Foo.^add_method('VAL', Foo.^method_table<val>);
    Foo.^add_method('RW', Foo.^method_table<rw>);
    Foo.^add_method('TYPED', Foo.^method_table<typed>);
}

my $f = Foo.new;
is $f.VAL, 5, 'the alias of a read-only accessor reads the attribute';
is Foo.new(val => 7).VAL, 7, 'an initialized value reads through the alias';
is $f.TYPED, 3, 'a typed attribute';
$f.RW = 9;
is $f.rw, 9, 'assigning through the alias of an rw accessor reaches the attribute';
is $f.RW, 9, 'and reads back';
ok Foo.^can('VAL'), 'the alias is a method of the class';
throws-like { $f.VAL = 1 }, X::Assignment::RO, 'the alias of a read-only accessor stays read-only';
