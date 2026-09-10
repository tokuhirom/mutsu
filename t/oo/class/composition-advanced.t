use Test;

plan 6;

sub double ($x) { $x * 2 };
sub invert ($x) { 1 / $x };

is (&invert o &double)(0.25), 2, 'parenthesized composed call is parsed and evaluated';
is (* o (* * 2))(* + 1)(3), 7, 'autocurry with Whatever on left composition operand';

my &one = sub ($a --> Str) { $a.uc } o sub { "$^a:$^b" };
is-deeply <a b c d>.map(&one).List, ("A:B", "C:D"), 'composition keeps RHS 2-arg arity for map';

is-deeply ([o] {$_ xx 2} xx 2)(3).List, ((3, 3), (3, 3)), '[o] reduction composes callables';
is-deeply ([∘] {$_ xx 2} xx 2)(3).List, ((3, 3), (3, 3)), '[∘] reduction composes callables';

my &z = infix:<o>;
my $f := z Failure.new;
is $f.handled, False, 'infix:<o> identity does not mark Failure as handled';
