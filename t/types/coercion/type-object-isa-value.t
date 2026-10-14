use Test;

plan 7;

# Mu.isa(Mu \type): a defined value argument stands for its own type.
is Int.isa(3), True, 'Int.isa(an Int value)';
is Int.isa(3.5), False, 'Int.isa(a Rat value)';
is Str.isa("x"), False, 'a Str argument names a type';
is Int.isa("Int"), True, 'a Str naming the type';
is Int.isa(Int), True, 'a type object argument';

class A { }
is A.isa(3), False, 'user class type object, unrelated value';
is A.new.isa(3), False, 'user class instance, unrelated value';

# vim: expandtab shiftwidth=4
