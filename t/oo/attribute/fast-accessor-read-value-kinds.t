use Test;

# The fast accessor read consults the attribute declaration only for an
# Array/Hash value (#10229): pin that scalar reads, the decontainerizing
# generated accessor, `is rw` item-ness and typed aggregates all still agree,
# and that a wrapped method still dispatches through its wrapper.

plan 9;

class C {
    has $.s = 42;
    has $.list = [1, 2, 3];
    has $.map = { a => 1 };
    has $.rw-list is rw = [1, 2];
    has Int @.ints = 1, 2;
    method via-self-list { my $n = 0; $n++ for self.list; $n }
    method via-dollar-list { my $n = 0; $n++ for $.list; $n }
}

my $c = C.new;
is $c.s, 42, 'plain scalar accessor read';
my $n = 0; $n++ for $c.list;
is $n, 3, '$obj.x decontainerizes an Array held in a $ attribute';
is $c.via-self-list, 3, 'self.x decontainerizes too';
is $c.via-dollar-list, 1, '$.x itemizes';
is $c.map.elems, 1, 'Hash in a $ attribute reads back';
$n = 0; $n++ for $c.rw-list;
is $n, 1, 'an is rw accessor keeps the Scalar item-ness';
throws-like { $c.ints.push('x') }, X::TypeCheck,
    'a typed @ attribute read keeps its element type';

class W { has $.x = 7; method m { 'orig' } }
W.^find_method('m').wrap(-> $self { 'wrapped ' ~ callsame });
is W.new.m, 'wrapped orig', 'a wrapped method dispatches through its wrapper';
is W.new.x, 7, 'an accessor read next to a wrapped method';
