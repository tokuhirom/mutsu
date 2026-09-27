use Test;

# `for $obj.attr <-> $v { $v = ... }` aliases the attribute's own container, as
# `my $c := $obj.attr` does, for the literal and the dynamic (`."$name"()`)
# accessor spelling alike, and for the default `$_` topic. Found via the
# Math::Symbolic distribution, which resolves an operation table's string
# references in place with `for $func."$prop"() <-> $val { $val = %by{$val} }`.

plan 8;

class F { has $.inverse is rw; has $.items is rw = (1, 2, 3) }

my $f = F.new(inverse => 'x');
my $name = 'inverse';

for $f.inverse <-> $v { $v = 'A' }
is $f.inverse, 'A', '<-> over a literal rw accessor writes through';

for $f."$name"() <-> $v { $v = 'B' }
is $f.inverse, 'B', '<-> over a dynamic rw accessor writes through';

for $f.inverse -> $v is rw { $v = 'C' }
is $f.inverse, 'C', 'an is rw loop parameter writes through';

for $f.inverse { $_ = 'D' }
is $f.inverse, 'D', 'the $_ topic writes through';

my $c := $f."$name"();
$c = 'E';
is $f.inverse, 'E', ':= bind to a dynamic rw accessor aliases the attribute';

my @seen;
for $f.items { @seen.push: $_ }
is @seen.elems, 1, 'a scalar attribute holding a List is still one item';

# A fresh object, so the dynamic spelling is the first thing that asks for
# the attribute's container.
my $g = F.new(inverse => 'y');
for $g."$name"() <-> $v { $v = 'Z' }
is $g.inverse, 'Z', 'the dynamic spelling works on a never-promoted attribute';

# Inside a package body, as the distribution's `unit class` module does.
package P {
    my $h = F.new(inverse => 'q');
    my $n = 'inverse';
    for $h."$n"() <-> $v { $v = 'W' }
    is $h.inverse, 'W', 'writes through inside a package body';
}
