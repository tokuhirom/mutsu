use v6;
use Test;

# Rakudo's Mu.clone copies `$` attributes but keeps an `@`/`%` attribute's
# container: an attribute with a default initializer (even an empty one) is
# bound at construction, so the clone shares it with the original, whereas an
# attribute nobody has initialized or filled is vivified separately by each.
# Seen in the JSON::RPC dist: `has @!stack = ( )` is filled through a clone.

plan 6;

class C {
    has $.f = 0;
    has @!a = ();
    has @!b;
    has %!h = ();
    method push { @!a.push: 1; @!b.push: 2; %!h<k> = 1; self }
    method n { "{@!a.elems} {@!b.elems} {%!h.elems}" }
}

my $o = C.new;
$o.clone.push;
is $o.n, '1 0 1', 'plain clone shares initialized @/% attributes, not the bare one';

my $o2 = C.new;
$o2.clone(:f(1)).push;
is $o2.n, '1 0 1', 'a clone with a twiddle does the same';

class D {
    has @!b;
    method touch { @!b.push: 0; self }
    method push { @!b.push: 2; self }
    method n { @!b.elems }
}
my $d = D.new.touch;
$d.clone.push;
is $d.n, 2, 'a filled bare @ attribute is shared by the clone';
my $e = D.new;
$e.clone.push;
is $e.n, 0, 'an untouched bare @ attribute is vivified separately';

class E {
    has $!s = [1];
    method push { $!s.push: 9; self }
    method n { $!s.elems }
}
my $x = E.new;
$x.clone.push;
is $x.n, 2, 'an itemized array in a $ attribute is shared';
my $y = C.new;
my $c = $y.clone;
is $c.f, 0, 'scalar attributes are copied';
