use Test;

# A subscript store through a method accessor (`$obj.h{...} = ...`) with a
# list-shaped index. Found through Pod::To::HTML, whose
# `$!renderer.crossrefs{$_} = $text for @indices` stores one itemized array
# per key and died with "Multi-dimensional index on non-array container".

plan 9;

class C { has @.a; has %.h is default(0) }

my $c = C.new;

$c.a[0, 1] = 5, 6;
is-deeply $c.a, [5, 6], 'positional slice through an accessor stores element-wise';

$c.a[0..2] = 1, 2, 3;
is-deeply $c.a, [1, 2, 3], 'Range subscript through an accessor is a slice';

$c.a[1, 2] = 9;
is-deeply $c.a, [1, 9, Any], 'slice past the end of the RHS stores Nil (the default)';

$c.h{"x", "y", "z"} = 2, 3;
is-deeply $c.h, {:x(2), :y(3), :z(0)}, 'associative slice pads with the hash default';

my $k = ["p", "q"];
$c.h{$k} = 1;
is $c.h{"p q"}, 1, 'an itemized array index is ONE key (its .Str)';

$c.h = ();
$c.h{$_} = 7 for [["u"], ["v", "w"]];
is-deeply $c.h, {:u(7), "v w" => 7}, 'topic bound to an inner array is one key';

my @k = <m n>;
$c.h{@k} = 4, 5;
is-deeply ($c.h<m>, $c.h<n>), (4, 5), 'an array variable index slices';

my $x = [10, 20];
$c.h{"s", "t"} = $x;
is-deeply ($c.h<s>, $c.h<t>), (10, 20), 'slice RHS flattens an itemized array';

is-deeply ($c.a[0, 1] = 7, 8), (7, 8), 'slice store evaluates to the stored List';
