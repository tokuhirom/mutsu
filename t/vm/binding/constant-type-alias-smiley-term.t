use Test;

# A constant that aliases a type is a term for that type, so a definiteness
# smiley on it names the aliased type: `Alias:D` is `Foo:D`. (NativeCall exports
# its C types this way: `my constant CArray = NativeCall::Types::CArray`.)
#
# Every expectation was verified against Rakudo.

plan 10;

my class Foo { }
my constant Bar = Foo;
my constant Baz = Bar;

is Bar.^name, 'Foo', 'the alias names the class';
is (Bar:D).^name, 'Foo:D', ':D keeps the aliased type';
is (Bar:U).raku, 'Foo:U', ':U in .raku';
is (Baz:D).^name, 'Foo:D', 'through a chain of aliases';
is (Bar:D).WHAT.^name, 'Foo:D', '.WHAT.^name';
ok Foo.new ~~ Bar:D, 'an instance matches the :D alias';
nok Foo ~~ Bar:D, 'the type object does not';
ok Foo ~~ Bar:U, 'but matches :U';

sub takes(Bar:D $x) { 'ok' }
is takes(Foo.new), 'ok', 'a parameter typed Alias:D binds an instance';
dies-ok { takes(Foo) }, 'and refuses the type object';
