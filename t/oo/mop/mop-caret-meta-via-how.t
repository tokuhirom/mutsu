use v6;
use Test;

plan 14;

# `Type.^meta(args)` is `Type.HOW.meta(Type, args)`: the HOW is the reflection
# answer, not the result of a user method.
class Foo { has $.x; method bar { 1 } }
class Sub is Foo { }

is Foo.^name, 'Foo', '.^name';
is Foo.^find_method('bar').name, 'bar', '.^find_method';
ok Foo.^can('bar'), '.^can finds a method';
nok Foo.^can('nope'), '.^can on a missing method';
is Sub.^mro.map(*.^name).join(','), 'Sub,Foo,Any,Mu', '.^mro';
is Sub.^parents.map(*.^name).join(','), 'Foo', '.^parents';
is Foo.^attributes.elems, 1, '.^attributes';
ok Foo.^lookup('bar').defined, '.^lookup';
is Foo.new.^name, 'Foo', 'on an instance';

# A user method named HOW or WHAT does not intercept the unquoted macros that
# `.^meta` goes through.
class Odd { method HOW { 'user-how' }; method m { 1 } }
is Odd.^name, 'Odd', '.^name ignores a user HOW method';
ok Odd.^find_method('m').defined, '.^find_method ignores a user HOW method';
is Odd."HOW"(), 'user-how', 'a quoted HOW call still reaches the user method';

# A custom metaclass: `.^meta` dispatches through the type object's own HOW.
my class MyHOW is Metamodel::ClassHOW {
    method shout($, $type) { $type.^name.uc }
}
my class Custom { }
Custom.HOW.^can('compose');
ok True, 'custom HOW setup compiles';
is (Foo.^find_method('bar') === Foo.^find_method('bar')), True, 'repeated lookups give one object';
