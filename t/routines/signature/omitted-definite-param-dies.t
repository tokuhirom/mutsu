use Test;

# An omitted optional `:D` parameter with no default cannot bind its type
# object: the call dies with X::Parameter::InvalidConcreteness, for a named
# or a positional parameter, in a sub or a method (#18075).

plan 9;

sub f(Str:D :$d) { $d.raku }
class C {
    method m(Int:D :$i) { $i.raku }
    method p(Int:D $x?) { $x.raku }
}
sub g(Str:D $x?) { $x.raku }

throws-like { f() }, X::Parameter::InvalidConcreteness,
    message => /"Parameter '\$d' of routine 'f' must be an object instance of type 'Str'"/,
    'an omitted :D named parameter of a sub';
throws-like { C.new.m }, X::Parameter::InvalidConcreteness,
    message => /"Parameter '\$i' of routine 'm'"/,
    'an omitted :D named parameter of a method';
throws-like { g() }, X::Parameter::InvalidConcreteness,
    'an omitted :D positional parameter of a sub';
throws-like { C.new.p }, X::Parameter::InvalidConcreteness,
    'an omitted :D positional parameter of a method';

is f(:d<x>), '"x"', 'a supplied :D named parameter binds';
is C.new.m(:i(4)), '4', '... in a method too';

sub h(Int:D :$i = 3) { $i }
is h(), 3, 'a :D named parameter with a default binds the default';
sub u(Int:U :$u) { $u.raku }
is u(), 'Int', 'an omitted :U named parameter binds its type object';
sub plain(Int :$i) { $i.raku }
is plain(), 'Int', 'an omitted named parameter without a smiley binds its type object';
