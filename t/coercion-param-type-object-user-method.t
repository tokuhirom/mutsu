use Test;

# A coercion-type parameter (`Str() $x`) calls the named method on its
# argument, and a method call on a TYPE OBJECT dispatches like any other. So a
# class that defines `method Str` coerces through it even when the argument is
# the bare type object -- it does not fall back to the "" a plain type object
# stringifies to.
#
# This is what the real `Test.rakumod`'s `like`/`unlike` rely on: they declare
# `Str() $got`, and roast/S24-testing/14-like-unlike.t passes them an anonymous
# class with a `Str` method.
#
# Verified assertion-for-assertion against rakudo.

plan 14;

class WithStr    { method Str { 'foo' } }
class WithInt    { method Int { 99 } }
class WithNum    { method Num { 2.5e0 } }
class Plain      { }

sub takes-str(Str() $g) { $g }
sub takes-int(Int() $g) { $g }
sub takes-num(Num() $g) { $g }

# --- the type object dispatches its class's coercion method ---------------
is takes-str(WithStr), 'foo',
   'Str() parameter coerces a type object through its own .Str';
is takes-str(WithStr.new), 'foo',
   'Str() parameter still coerces an instance through its .Str';

is takes-int(WithInt), 99,
   'Int() parameter coerces a type object through its own .Int';
is takes-int(WithInt.new), 99,
   'Int() parameter still coerces an instance through its .Int';

is takes-num(WithNum), 2.5e0,
   'Num() parameter coerces a type object through its own .Num';

# An anonymous class is the shape roast/S24-testing/14-like-unlike.t uses.
is takes-str(class { method Str { 'zap' } }), 'zap',
   'Str() parameter coerces an anonymous type object through its .Str';

# The coerced value really is of the target type, not the original.
isa-ok takes-str(WithStr), Str,
   'the bound value is a Str, not the type object';

# --- a class without the method is unaffected -----------------------------
# A bare type object stringifies to "" (with a warning); that must not change.
quietly {
    is takes-str(Plain), '', 'a type object with no .Str still coerces to ""';
    is takes-str(Int),   '', 'Int still coerces to ""';
    is takes-str(Any),   '', 'Any still coerces to ""';
}

# --- ordinary values are unaffected ---------------------------------------
is takes-str(42),    '42', 'an Int value still coerces to its text';
is takes-str('s'),   's',  'a Str value passes through';
is takes-int('7'),   7,    'a numeric string still coerces to Int';
is takes-int(3.9),   3,    'a Rat still truncates to Int';

# vim: expandtab shiftwidth=4
