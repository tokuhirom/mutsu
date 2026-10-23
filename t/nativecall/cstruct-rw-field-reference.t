use Test;
use NativeCall;

plan 8;

class Rect is rw is repr('CStruct') { has num32 $.top; has int32 $.n; has uint8 $.u; }

# An `is rw` method or sub returning a native CStruct field hands out an
# assignable location into the struct's memory (Rakudo's native attribute
# reference).
class Box {
    has Rect $!content .= new;
    multi method top is rw { $!content.top }
    method set { $.top = 5e0; $.top }
}
is Box.new.set, 5, 'an rw method returning a num32 field is assignable';

sub gettop($rect) is rw { $rect.top }
my $r = Rect.new;
gettop($r) = 7e0;
is $r.top, 7, 'an rw sub returning a num32 field is assignable';

sub getn($rect) is rw { $rect.n }
getn($r) = -3;
is $r.n, -3, 'an int32 field';
sub getu($rect) is rw { $rect.u }
getu($r) = 200;
is $r.u, 200, 'a uint8 field';

# The reference reads and writes the live struct.
my $ref := $r.n;
$r.n = 11;
is $ref, 11, 'a bound reference sees a later direct write';
$ref = 12;
is $r.n, 12, 'and a write through it reaches the struct';
is $ref.VAR.^name, 'IntAttrRef', 'the container is an IntAttrRef';

# Direct assignment still works.
$r.top = 2e0;
is $r.top, 2, 'direct assignment to a field';
