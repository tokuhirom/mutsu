use Test;

plan 8;

# A user class may inherit from CompUnit::DependencySpecification (Pakku::Spec).
class Foo is CompUnit::DependencySpecification { method hi { $.short-name } }
is Foo.new(:short-name<Bar>).hi, 'Bar', 'subclass method reads the inherited short-name';

class Baz is CompUnit::DependencySpecification { has $.x }
my $b = Baz.new(:short-name<Q>, :x(3));
is $b.x, 3, 'subclass attribute is initialised';
is $b.short-name, 'Q', 'inherited short-name accessor';
isa-ok $b, CompUnit::DependencySpecification, 'subclass instance isa the base type';
is $b.^name, 'Baz', 'the instance keeps the subclass type';

# An overriding matcher reaches the base implementation through callsame.
class Pinned is CompUnit::DependencySpecification {
    method version-matcher { callsame }
}
my $p = Pinned.new(:short-name<P>, :version-matcher<1.2>);
is $p.version-matcher, '1.2', 'callsame from an overriding version-matcher reaches the base';
is Pinned.new(:short-name<P>).version-matcher, True, 'an omitted matcher stays True';

is CompUnit::DependencySpecification.new(:short-name<Z>).short-name, 'Z',
    'the base type is still constructible';
