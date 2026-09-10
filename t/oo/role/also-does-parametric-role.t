use Test;

role Typed[::T] {
    my $.marker = 'role';
    has T $.value is rw;
    has $.fallback is default(T.^name) is rw;

    method type-name-from-method { T.^name }
    method accept(T:D $value) { $value }
}

class Base { }
class IntConsumer is Base {
    also does Typed[Int];
    method local-value { $!value }
}

is IntConsumer.new.type-name-from-method, 'Int',
    '`also does Role[Args]` binds the role type parameter in methods';
is IntConsumer.marker, 'role',
    'a role class-level attribute is carried through body composition';
is IntConsumer.new(value => 42).value, 42,
    'a role attribute type constraint is substituted';
is IntConsumer.new(value => 42).local-value, 42,
    'later class methods can access attributes from the body-composed role';
dies-ok { IntConsumer.new(value => 'wrong') },
    'the substituted role attribute type constraint is enforced';
is IntConsumer.new.accept(7), 7,
    'a substituted method parameter accepts the role argument type';
dies-ok { IntConsumer.new.accept('wrong') },
    'a substituted method parameter rejects another type';
is IntConsumer.new.fallback, 'Int',
    'a role attribute default expression is carried through body composition';

role ValueRole[$value] {
    method role-value { $value }
}

class ComputedConsumer {
    also does ValueRole[1 + 2];
}

is ComputedConsumer.new.role-value, 3,
    'a body role argument expression is evaluated from its compiled chunk';

role Plain {
    has $.answer is default(42) is rw;
}

class PlainConsumer {
    also does Plain;
}

is PlainConsumer.new.answer, 42,
    'non-parametric `also does` carries role attribute traits too';

done-testing;
