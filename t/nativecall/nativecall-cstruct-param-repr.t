use Test;
use NativeCall;

# Net::Ethereum (via Compress::Bzip2::Raw): `sub f(bz_stream, ...) is native`
# with a file-scoped `my class bz_stream is repr('CStruct')` warned
# "Not an accepted NativeCall type" because a signature parameter's `.type`
# was the bare spelling, not the lexical class, so `.REPR` was P6opaque.

plan 5;

my class Point is repr('CStruct') {
    has int32 $.x;
    has int32 $.y;
}

sub takes-point(Point, int32) { }
sub takes-named(Point $p, int32 $n) { }

my $type = &takes-point.signature.params[0].type;
is $type.REPR, 'CStruct', 'positional param type keeps the CStruct REPR';
ok $type === Point, 'param type is the lexical class itself';
is &takes-named.signature.params[0].type.REPR, 'CStruct', 'named param too';
is Point.REPR, 'CStruct', 'class REPR';

my $warned = False;
{
    CONTROL { when CX::Warn { $warned = True; .resume } }
    sub strlen-like(Point, int32 --> int32) is native { * }
}
nok $warned, 'declaring a native sub taking a CStruct does not warn';

done-testing;
