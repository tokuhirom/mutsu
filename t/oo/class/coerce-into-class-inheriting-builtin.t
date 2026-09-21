use Test;

# From issue #8856. `class C is Str {}; C('x')` coerces through the built-in
# parent's INHERITED `COERCE`, building a real `C` that carries the string --
# the class twin of `role Markup is Str {}` coercion
# (t/oo/role/coerce-into-role-inheriting-builtin.t), which needed no built-in
# -backed instance representation because a non-Instance value already rides
# in mutsu's mixin wrapper. A class instance has no such wrapper to reuse; see
# src/runtime/types/native_backed_class.rs.

plan 14;

class CM is Str { }

# The call spelling.
my $m = CM('hello');
is $m.^name, 'CM', 'coercing into a class that inherits Str builds an instance of that class';
is $m.Str, 'hello', '... carrying the coerced string';
ok $m ~~ CM, '... which does the declared type';
ok $m ~~ Str, '... and is still a Str';
is "interp $m", 'interp hello', '... and interpolates as the string';
is $m.raku, '"hello"', '... and .raku reprs as the inherited Str.raku would';

# The explicit `.COERCE` spelling.
is CM.COERCE('hello').Str, 'hello', '.COERCE(value) coerces the same way';

# `.^lookup` reports the method as inherited from the built-in parent, not
# absent.
is CM.^lookup('COERCE').package.^name, 'Str',
    '.^lookup finds COERCE inherited from the built-in parent';

# The return-type spelling.
sub mk(--> CM()) { 'yo' }
is mk().Str, 'yo', 'a `--> Class()` return type coerces the same way';
ok mk() ~~ CM, '... and the returned value is the declared class';

# A non-Str built-in parent, to show the rule is not Str-specific.
class Counted is Int { }
my $c = Counted(42);
is $c + 1, 43, 'a class inheriting Int coerces an Int and stays numeric';
ok $c ~~ Counted, '... and is the declared class';

# A class with no built-in parent is untouched: still an impossible coercion.
class Plain { }
dies-ok { Plain('x') },
    'a class with no built-in parent still refuses to coerce';

# A class that already declares its own COERCE/new wins over the inherited
# built-in behavior.
class Custom is Str {
    method COERCE($v) { self.new }
}
is Custom('anything').Str, '', 'a class-declared COERCE wins over the inherited built-in one';
