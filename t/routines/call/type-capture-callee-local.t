use Test;

# A routine's type capture (`::Type`) is its own: a callee capturing under
# the same name does not overwrite the caller's (#13568, found on the Deps
# distribution).
plan 5;

sub inner(::Type) { Type }
sub inner-typed(::Type $x) { $x }

sub plain(::Type) { inner(Int); Type }
is plain(Str).^name, 'Str', 'a callee\'s same-named capture leaves the caller\'s';

sub typed(::Type $v) { inner-typed(42); Type }
is typed('s').^name, 'Str', 'also for a capture on a typed parameter';

sub through-closure(::Type) { -> |c { inner(Int); Type } }
is through-closure(Str)().^name, 'Str', 'a closure reads its routine\'s capture after the call';

class K { method m(::Type) { inner(Int); Type.^name } }
is K.m(Rat), 'Rat', 'a method\'s capture is its own too';

is inner(Rat).^name, 'Rat', 'the callee still sees its own capture';
