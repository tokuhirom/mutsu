use v6;
use nqp;
use Test;

# Hash2Class (zef:lizmat): `Hash2Class::List is List` installs another list's
# storage with nqp::bindattr(self, List, '$!reified', ...) on an instance made
# by nqp::create, and then reads it through ordinary List methods.

plan 5;

my class L is List {
    method !SET-SELF(\array) {
        nqp::bindattr(self, List, '$!reified', nqp::getattr(nqp::decont(array), List, '$!reified'));
        self
    }
    method new(\array) { nqp::create(self)!SET-SELF(array) }
}

my @a = 10, 20, 30;
my $l = L.new(@a);
is $l.elems, 3, '.elems sees the bound storage';
is $l[1], 20, 'indexing sees the bound storage';
is $l.join(','), '10,20,30', 'List methods see the bound storage';
is nqp::elems(nqp::getattr(nqp::decont($l), List, '$!reified')), 3, '$!reified reads back';
isa-ok $l, List, 'still a List';
