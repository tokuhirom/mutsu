use Test;
use nqp;

# A class whose BUILDPLAN a HOW mixin replaced keeps the `is default(...)`
# values of its attributes: the default belongs to the attribute's container,
# so it holds without a plan task (#14266, AttrX::Mooish).

plan 5;

my role Hook {
    method create_BUILDPLAN(Mu \type) {
        callsame;
        my \plan := nqp::getattr(self, Metamodel::ClassHOW, '@!BUILDPLAN');
        my \newplan := nqp::create(nqp::what(plan));
        for ^nqp::elems(plan) -> $i { nqp::push(newplan, nqp::atpos(plan, $i)) }
        nqp::bindattr(self, Metamodel::ClassHOW, '@!BUILDPLAN', newplan);
    }
}

class Base {
    has $.base is default('b');
    BEGIN { Base.HOW does Hook }
}
class Derived is Base {
    has $.initial is default(pi);
    has $.count is default(7) = 3;
    has Int $.typed is default(9);
}

my $o = Derived.new;
is $o.initial, pi, 'an `is default` attribute holds its default';
is $o.base, 'b', 'an inherited `is default` attribute holds its default';
is $o.count, 3, 'an explicit initializer wins over the `is default`';
is $o.typed, 9, 'a typed `is default` attribute holds its default';
is Derived.new(:initial(2)).initial, 2, 'a named argument wins';
