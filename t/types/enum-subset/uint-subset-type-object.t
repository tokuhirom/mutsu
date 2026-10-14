use Test;

plan 7;

# UInt is a subset of Int: its type object dispatches Int's methods.
is UInt.Range.raku, '0..^Inf', 'UInt.Range';
is Int.Range.raku, '-Inf^..^Inf', 'Int.Range is unchanged';
is UInt.^mro.raku, '(UInt, Int, Cool, Any, Mu)', 'UInt.^mro';
ok UInt ~~ Int, 'UInt smartmatches Int';
ok UInt.isa(Int), 'UInt.isa(Int)';
ok 5 ~~ UInt, 'a non-negative Int is a UInt';
nok -1 ~~ UInt, 'a negative Int is not';

# vim: expandtab shiftwidth=4
