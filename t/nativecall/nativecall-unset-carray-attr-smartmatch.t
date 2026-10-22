use NativeCall;
use Test;

plan 6;

# An unset `has CArray[uint16] $.k` holds the same type object as a typed
# `my` variable, so it smartmatches its own parameterized type (#14441).
class W { has CArray[uint16] $.kids }
my CArray[uint16] $x;

ok W.new.kids ~~ CArray[uint16], 'unset attribute ~~ its parameterized type';
ok $x ~~ CArray[uint16], 'typed my variable ~~ the same type';
ok W.new.kids === CArray[uint16], 'identical to the type object';
nok W.new.kids ~~ CArray[int32], 'but not another instantiation';
is W.new.kids.^name, 'NativeCall::Types::CArray[uint16]', '.^name';
is-deeply W.new.kids.^mro.map(*.^name).List,
    ('NativeCall::Types::CArray[uint16]', 'NativeCall::Types::CArray', 'Any', 'Mu'), '.^mro';
