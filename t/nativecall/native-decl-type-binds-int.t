use Test;

# A `native`-declared type (upstream NativeCall::Types' shape) binds a value
# of the type its REPR boxes to, as a core native type does (#11555).

plan 4;

our native mysize is Int is ctype<size_t> is unsigned is repr<P6int> { }
our native mydbl is Num is ctype<double> is repr<P6num> { }

sub take-size(mysize $n --> mysize) { $n }
sub take-dbl(mydbl $d) { $d }

is take-size(5), 5, 'an Int binds to a P6int native declaration';
is take-dbl(1.5e0), 1.5e0, 'a Num binds to a P6num native declaration';
dies-ok { EVAL q[take-size('x')] }, 'a Str does not bind to a P6int one';
nok 5 ~~ mysize, 'smartmatch stays False, as for a core native';
