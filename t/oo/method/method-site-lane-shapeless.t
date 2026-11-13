use Test;

# `.WHICH` / `.defined` / `.DEFINITE` on a receiver with no built-in dispatch
# shape (a user class, its instances, a role type object, `Any`) is answered by
# the method-table call-site lane, keyed by the receiver's type. A user method
# of the same name in the type or a parent must still win, and every call of a
# repeated site must give the same answer as the first.

plan 14;

class C { }
class D { method WHICH { 'D-own' } }
class E is D { }
role R[::T] { }
role HasDefined { method defined { 'role-defined' } }
class F does HasDefined { }

my @which;
@which.push: C.WHICH for ^3;
is @which[0], @which[2], 'a repeated site gives one type identity';
is C.WHICH.^name, 'ValueObjAt', 'a type object WHICH is a ValueObjAt';
is C.WHICH.Str, @which[1].Str, 'the identity is stable';

is D.WHICH, 'D-own', 'a user WHICH on the class wins';
is E.WHICH, 'D-own', 'an inherited user WHICH wins on a type object';
is E.new.WHICH, 'D-own', 'an inherited user WHICH wins on an instance';
is F.defined, 'role-defined', 'a role method of the same name wins';

is-deeply (C.defined, C.new.defined), (False, True), 'defined';
is-deeply (C.DEFINITE, C.new.DEFINITE), (False, True), 'DEFINITE';

ok R.WHICH.Str.starts-with('R'), 'a role type object has its own identity';
ok Any.WHICH.Str.starts-with('Any|'), 'Any.WHICH';

my $c = C.new;
is $c.WHICH, $c.WHICH, 'an instance keeps its identity';
isnt C.new.WHICH, $c.WHICH, 'two instances differ';

sub w(Mu \M) { M.WHICH }
is (w(C), w(D), w(C)).map(*.Str).join(','), (C.WHICH, 'D-own', C.WHICH).join(','),
    'one site alternating between types answers each by its own type';
