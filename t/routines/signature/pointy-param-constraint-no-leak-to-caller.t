use Test;

plan 4;

# A typed pointy-block parameter's constraint must not be written back over a
# same-named typed lexical in the calling routine (#9965).
sub f1 { my &b = -> Str $h { $h.chars }; b("a") }
sub g1 { my UInt $h = f1(); $h }
is g1(), 1, 'Str pointy param does not leak into caller $h';

sub f2 { my &b = -> blob8 $h { $h.elems }; b(blob8.new(1)) }
sub g2 { my UInt $h = f2(); $h }
is g2(), 1, 'blob8 pointy param does not leak into caller $h';

sub f3 { reduce -> blob8 $h, $t { blob8.new($h[0] + $t) }, blob8.new(1), |^3 }
sub g3 { my UInt $h = f3().elems; $h }
is g3(), 1, 'blob8 reduce block param does not leak into caller $h';

sub g4 { my UInt $h = 5; f2(); $h = -1 }
throws-like { g4() }, X::TypeCheck::Assignment, 'the caller keeps its own UInt constraint';
