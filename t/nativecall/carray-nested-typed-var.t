use v6;
use Test;
use NativeCall;

plan 8;

my CArray[CArray[uint8]] $v;
is $v.^name, 'NativeCall::Types::CArray[NativeCall::Types::CArray[uint8]]',
    'a variable typed with a nested CArray parameterisation has the qualified name';
is $v.REPR, 'CArray', 'its REPR is CArray';
ok $v === CArray[CArray[uint8]], 'it is the CArray[CArray[uint8]] type object';

ok CArray[uint8].^can('of').so, 'CArray[T].^can("of") is true';
ok CArray[CArray[uint8]].^can('of').so, 'so is a nested parameterisation';
is CArray[CArray[uint8]].of.^name, 'NativeCall::Types::CArray[uint8]',
    '.of of a nested CArray keeps the inner parameterisation';
is CArray[uint8].of.^name, 'uint8', '.of of a flat CArray is the element type';
nok CArray[uint8].^can('nonesuch').so, '.^can of an unknown method stays false';
