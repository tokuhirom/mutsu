use Test;

# A `my class` in a routine runs its body on every entry; a statement of the
# body reads the `my` this entry's body just declared, not the value the
# previous entry left (#18088).

plan 5;

my @seen;
sub f($i) {
    my class C { my $z = $i; @seen.push: "z=$z i=$i"; method z { $z } };
    C
}
f(1); f(2);
is @seen.join(' | '), 'z=1 i=1 | z=2 i=2', 'each entry reads its own body my';
is f(3).z, 3, 'a method reads the latest entry';
is @seen[*-1], 'z=3 i=3', '... after the body read it too';

my @w;
sub g($i) { my class D { my $z = $i; my $w = $z + 1; @w.push: $w }; D }
g(10); g(20);
is @w.join(','), '11,21', 'a later body my is computed from this entry';

my @a;
sub h($i) { my class E { my @l = $i, $i; @a.push: @l.sum }; E }
h(1); h(5);
is @a.join(','), '2,10', 'an array body my is read fresh too';
