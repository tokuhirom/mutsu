use v6;
use Test;

plan 4;

# A word the parse spelled as an imported term (a native type, an imported
# constant) is resolved where it stands by finalization (ADR-15760 slice 2).

use NativeCall;

my uint16 $a = 65535;
is $a, 65535, 'a native type word declares a typed variable';
is int32.^name, 'int32', 'a native type word is a type object';
is (num64 ~~ Num), True, 'a native type word smartmatches its base';

sub f(int64 $x) { $x + 1 }
is f(4), 5, 'a native type word in a signature';
