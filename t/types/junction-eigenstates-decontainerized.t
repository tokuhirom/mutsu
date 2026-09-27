use Test;

# Junction eigenstates are values, not the source's containers. After `grep`
# or `first` aliased `$_` to an array's elements (promoting them to shared
# cells), a junction built from the array held those cells, and a method
# threaded over it died with "No such method". Found via the Math::Symbolic
# distribution (`$node.children.all.type eq 'value'`).

plan 4;

class T { has $.type }

my @c = T.new(type => 's'), T.new(type => 'v');
my @g = @c.grep({ .type eq 's' });

ok any(@c).type eq 's', 'any() over grep-aliased elements threads a method';
nok @c.all.type eq 's', '.all over grep-aliased elements threads a method';
ok (@c[0] | @c[1]).type eq 'v', 'infix | over aliased elements threads a method';

my @d = T.new(type => 'a');
my $first = @d.first({ .type eq 'a' });
is @d.one.type.so, True, '.one after first() threads a method';
