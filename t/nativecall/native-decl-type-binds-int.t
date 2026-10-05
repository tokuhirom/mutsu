use Test;

# A `native`-declared type (upstream NativeCall::Types' shape) binds a value
# of the type its REPR boxes to, as a core native type does (#11555).

plan 8;

our native mysize is Int is ctype<size_t> is unsigned is repr<P6int> { }
our native mydbl is Num is ctype<double> is repr<P6num> { }
our native my8 is Int is nativesize(8) is repr<P6int> { }

sub take-size(mysize $n --> mysize) { $n }
sub take-dbl(mydbl $d) { $d }
sub take-i8(my8 $n) { $n }
class NativeI8ParamProbe { method bind(my8 $n) { $n } }

is take-size(5), 5, 'an Int binds to a P6int native declaration';
is take-dbl(1.5e0), 1.5e0, 'a Num binds to a P6num native declaration';
is take-size(True), 1, 'Bool unboxes when bound to an unsigned P6int declaration';
is take-i8(True), 1, 'Bool unboxes when bound to a sized P6int declaration';
is take-i8(300), 44, 'a sized P6int declaration wraps to its signed width';
is NativeI8ParamProbe.new.bind(300), 44, 'method parameters use the declared native width';
dies-ok { EVAL q[take-size('x')] }, 'a Str does not bind to a P6int one';
nok 5 ~~ mysize, 'smartmatch stays False, as for a core native';
