use v6;
use lib 't/lib';
use Test;

plan 2;

my sub thing(Str() $s) { "outer($s)" }

sub wrapper() {
    use ImportedProtoShadow;
    "wrapped:" ~ thing("x");
}

is wrapper(), 'wrapped:inner3(x)',
    'an imported proto/multi family shadows an enclosing sub';
is thing('y'), 'outer(y)',
    'the enclosing sub remains after the importing routine returns';
