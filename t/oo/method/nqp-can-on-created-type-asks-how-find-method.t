use Test;
use nqp;

plan 3;

# A type made by Metamodel::Primitives.create_type has no methods of its own:
# nqp::can asks its HOW's find_method, and a HOW without one dies.
my class BareHOW {
    my constant ARCHETYPES = Metamodel::Archetypes.new: :nominal;
    method archetypes(::?CLASS:_: $? --> ARCHETYPES) { }
    method new_type(::?CLASS:_:) is raw {
        my BareHOW:D $meta := self.new;
        Metamodel::Primitives.create_type: $meta, 'Uninstantiable';
    }
}
my class KnowingHOW {
    my constant ARCHETYPES = Metamodel::Archetypes.new: :nominal;
    method archetypes(::?CLASS:_: $? --> ARCHETYPES) { }
    method find_method(Mu, Str $name) { $name eq 'known' ?? 1 !! Nil }
    method new_type(::?CLASS:_:) is raw {
        my KnowingHOW:D $meta := self.new;
        Metamodel::Primitives.create_type: $meta, 'Uninstantiable';
    }
}

dies-ok { nqp::can(BareHOW.new_type, 'raku') }, 'a HOW without find_method cannot answer';
is nqp::can(KnowingHOW.new_type, 'known'), 1, 'the HOW find_method finds a method';
is nqp::can(KnowingHOW.new_type, 'other'), 0, '...and reports an absent one';
