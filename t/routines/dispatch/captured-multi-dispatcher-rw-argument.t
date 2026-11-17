use lib 't/lib';
use Test;
use CapturedRwArgs::Ops;
use CapturedRwArgs::Caller;

# An imported multi passed as a code object into a module that cannot see the
# family by name is dispatched over its captured candidates. A variable
# argument must reach the `is rw` winner as a container: ranking the
# candidates ran user code (the left operand's `Str`) that consumed the call's
# argument sources, and the winner then died with "expects a writable
# container". Red's CHECK-built templates call its imported `infix:<lt>` this
# way (Red t/091-rakuast-compiler).

plan 2;

is call-with(&cap-lt, Tag.new), 'rw:2', 'a variable argument binds the is rw candidate';
is call-literal(&cap-lt, Tag.new), 'ro:3', 'a literal argument takes the readonly one';
