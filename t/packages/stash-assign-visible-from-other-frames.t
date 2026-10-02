use Test;

# `Pkg::<key> = v` / `Pkg::{$k} = v` store a symbol in the package's stash that
# every frame can read back, and `Pkg::<Name>` is a stash lookup, not a type name.
# Source: Logic::Ternary (zef distribution), whose `defined` method reads
# `Logic::Ternary::<Unknown>` after `sub EXPORT` stored it.

plan 9;

class A { }
A::<Z> = 5;
my $k = 'Y';
A::{$k} = 6;

sub in-sub { A::<Z> }
is in-sub(), 5, 'literal unsigiled key read from a sub';
sub in-sub-y { A::{'Y'} }
is in-sub-y(), 6, 'runtime key assignment read from a sub';
is A::<Z>, 5, 'read at the assigning scope';
is-deeply A::.keys.sort.List, ('Y', 'Z'), 'keys are listed from another frame';

class B {
    class C { }
    method z { B::<Z> }
}
B::<Z> = 7;
is B.z, 7, 'read from a method';
ok B::<C> === B::C, 'bare key finds a nested type';
ok !B::<Nope>.defined, 'a missing bare key is not a type name';
ok B::<Nope> === Any || B::<Nope> === Nil, 'a missing bare key is an undefined value';
is-deeply (B::<C>.new ~~ B::C), True, 'nested type is usable';
