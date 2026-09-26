use Test;

plan 10;

# The integer bitwise and shift operators numify an object operand through
# its `.Numeric` (not its `.Int`), as the prefix `+^` already did (#9566).

class C { method Numeric { 224 }; method Int { 100 } }

is C.new +> 2, 56, '+> uses .Numeric';
is C.new +& 7, 0, '+& uses .Numeric';
is C.new +< 1, 448, '+< uses .Numeric';
is 2 +| C.new, 226, '+| with the object on the right';
is 3 +^ C.new, 227, '+^ with the object on the right';
is +^C.new, -225, 'prefix +^ uses .Numeric';
is ([+&] C.new, 255), 224, 'the reduction form agrees';

{
    my $x = C.new;
    $x +>= 2;
    is $x, 56, '+>= on an object-holding scalar';
}

# The FixedInt shape: `Numeric` delegated to an attribute.
class D {
    has $.v handles <Numeric>;
}
is D.new(:v(224)) +> 2, 56, 'delegated Numeric via `handles`';
is D.new(:v(224)) +& 0xF0, 224, '... with +&';
