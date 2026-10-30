use v6;
use Test;

plan 12;

class Foo { has $.x; method WHAT { "user" }; method HOW { "user-how" } }
class Bar { has $.y }
role R { }
class Baz does R { }

# The unquoted MOP pseudo-methods are macros: a user method of the same name
# does not intercept them, on a type object or an instance.
is Foo.WHAT.^name, 'Foo', 'unquoted .WHAT on a type object ignores a user method';
is Foo.new.WHAT.^name, 'Foo', 'unquoted .WHAT on an instance ignores a user method';
is Foo.HOW.^name, 'Perl6::Metamodel::ClassHOW', 'unquoted .HOW on a type object';
is Foo.new.HOW.^name, 'Perl6::Metamodel::ClassHOW', 'unquoted .HOW on an instance';

# The quoted form is an ordinary method call and does reach the user method.
is Foo."WHAT"(), 'user', 'quoted .WHAT reaches the user method';
is Foo."HOW"(), 'user-how', 'quoted .HOW reaches the user method';

is Bar.HOW.name(Bar), 'Bar', '.HOW.name';
ok Bar.HOW.can(Bar, 'new'), '.HOW answers the metamodel methods';
is Bar.new(y => 1).WHAT.gist, '(Bar)', '.WHAT of a plain instance';
is Baz.HOW.^name, 'Perl6::Metamodel::ClassHOW', '.HOW of a class composing a role';
is 42.HOW.^name, 'Perl6::Metamodel::ClassHOW', '.HOW of a native value is unchanged';

# Repeated calls return the same metaobject.
my @h = (^3).map: { Bar.HOW.WHICH };
is @h.unique.elems, 1, 'repeated .HOW calls answer one metaobject';
